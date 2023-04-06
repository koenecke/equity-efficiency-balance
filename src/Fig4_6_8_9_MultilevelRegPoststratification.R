library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(scales)
library(lme4)
library(mlogit)
library(plm)
library(stargazer)
library(arm)
library(Hmisc)

# Import data
data_dir <- '~/data/'
plot_dir <- '~/plots/'

# Private Prolific survey data (subset to respondents passing attention checks)
prolific_df_clean <- read.csv(paste0(data_dir, 'prolific_data.csv')) 

# Public 2018 AP VoteCast data available at https://doi.org/10.3886/E109687V2
ap_vote_groups <- read.csv(paste0(data_dir, 'ap_vote_weights.csv'))

#################################################################################################
############### CLEAN SURVEY AND GET RESPONDENT-LEVEL SURVEY RESULTS ############################
#################################################################################################

# Extract comparisons presented to each survey taker
mode <- function(codes){
  which.max(tabulate(codes))
}

UserPrefPercents <- function(df, slope_val){
  
  cols <- paste0(slope_val, "_pair")
  
  if(slope_val == 'high'){
    leftmost_perc <- 8
    demoparity_perc <- 24
  } else if(slope_val == 'low'){
    leftmost_perc <- 10
    demoparity_perc <- 22
  }  else if(slope_val == 'equal'){
    leftmost_perc <- 8
    demoparity_perc <- 23
  }  else if(slope_val == 'fliplow'){
    leftmost_perc <- 8
    demoparity_perc <- 21
  }  else if(slope_val == 'fliphigh'){
    leftmost_perc <- 5
    demoparity_perc <- 21
  }
  print(leftmost_perc)
  print(demoparity_perc)
  
  two_choices <- df %>%
    filter(slope == slope_val) %>%
    dplyr::select(c('ResponseId', starts_with(cols))) %>%
    dplyr::select(c('ResponseId', contains("_DO"))) %>%
    gather(question, string, c(2:ncol(.)), factor_key=FALSE) %>%
    separate(string, into = c("option1", "option2"), "\\|")
  
  # Extract actual user selections
  user_selections <- df %>%
    filter(slope == slope_val) %>%
    dplyr::select(c('ResponseId', starts_with(cols))) %>%
    dplyr::select(c('ResponseId', !contains("_DO"))) %>%
    gather(question, selection, c(2:ncol(.)), factor_key=FALSE) %>%
    mutate(question = paste0(question,"_DO")) %>%
    left_join(two_choices, by=c('ResponseId', 'question')) %>%
    filter(!is.na(selection) & selection != "") %>%
    mutate(win1 = 1*(selection == option1),
           win2 = 1*(selection == option2))
  
  question_level_nums <- user_selections %>%
    mutate(ResponseId_Fac = as.factor(ResponseId),
           paren1 = str_locate_all(option1, pattern ='\\(+'),
           percent1 = str_locate_all(option1, pattern ='\\%+'),
           paren_idx1 = as.numeric(substr(as.character(paren1),3,4)),
           perc_idx1 = as.numeric(substr(as.character(percent1),3,4)),
           paren2 = str_locate_all(option2, pattern ='\\(+'),
           percent2 = str_locate_all(option2, pattern ='\\%+'),
           paren_idx2 = as.numeric(substr(as.character(paren2),3,4)),
           perc_idx2 = as.numeric(substr(as.character(percent2),3,4)),
           spanish_perc1 = as.numeric(substr(option1, paren_idx1+1, perc_idx1-1)),
           spanish_perc2 = as.numeric(substr(option2, paren_idx2+1, perc_idx2-1)),
           spanish_perc_difference = ifelse(win1 ==1,
                                            as.numeric(spanish_perc1 - spanish_perc2), #option 1 won
                                            as.numeric(spanish_perc2 - spanish_perc1)), #option 2 won
           spanish_binary_difference = as.numeric(spanish_perc_difference>0)) %>%
    dplyr::select(-c('paren1', 'percent1', 'paren_idx1', 'perc_idx1', 'paren2', 'percent2', 'paren_idx2', 'perc_idx2')) %>%
    mutate(selected_spanishperc = ifelse(win1 ==1,
                                         as.numeric(spanish_perc1), #option 1 won
                                         as.numeric(spanish_perc2)))
  
  user_level_percs <- question_level_nums %>%
    group_by(ResponseId) %>%
    summarise(avg_spanishperc = mean(selected_spanishperc),
              mode_spanishperc = mode(selected_spanishperc)) %>%
    mutate(leftmost_flag = ifelse(leftmost_perc == mode_spanishperc, 1, 0),
           closestdemoparity_flag = ifelse(demoparity_perc == mode_spanishperc, 1, 0),
           morethandemoparity_flag = ifelse(demoparity_perc <= mode_spanishperc, 1, 0),
           lessthandemoparity_flag = ifelse(demoparity_perc > mode_spanishperc, 1, 0))
  
  return(user_level_percs)
}
fliphigh_user_selections <- UserPrefPercents(prolific_df_clean, 'fliphigh')
fliplow_user_selections <- UserPrefPercents(prolific_df_clean, 'fliplow')
equal_user_selections <- UserPrefPercents(prolific_df_clean, 'equal')
low_user_selections <- UserPrefPercents(prolific_df_clean, 'low')
high_user_selections <- UserPrefPercents(prolific_df_clean, 'high')

# confirm each % option is chosen by someone in each subgroup
print(
  length(table(fliphigh_user_selections$mode_spanishperc))==6 &
    length(table(fliplow_user_selections$mode_spanishperc))==6 &
    length(table(equal_user_selections$mode_spanishperc))==6 &
    length(table(low_user_selections$mode_spanishperc))==6 &
    length(table(high_user_selections$mode_spanishperc))==6
)

# merge top preferred percentages back with demographic info
user_perc_prefs <- prolific_df_clean %>%
  left_join(rbind(fliphigh_user_selections,
                  fliplow_user_selections,
                  equal_user_selections,
                  low_user_selections,
                  high_user_selections
  ),
  by=c('ResponseId'))

# generate bins
user_perc_prefs <- user_perc_prefs %>%
  mutate(Gender_Bins = ifelse(gender %in% c("Prefer not to state"), "Unknown", 
                              ifelse(gender %in% c("Female", "Non-binary"), "NotMale", gender)),
         Age_Vals = ifelse(age==5, 50, ifelse(age=="Thirty four", 34, ifelse(age=="2domdom", 20, as.numeric(age)))),
         Age_Bins = cut(Age_Vals, breaks = c(18, 30, 40, 50, 100), include.lowest=TRUE, right=FALSE),
         Education_Bins = ifelse(education %in% c("Less than high school degree",
                                                  "High school graduate (high school diploma or equivalent including GED)"), "HighSchool",
                                 ifelse(education %in% c("Associate degree in college (2-year)","Some college but no degree"), 
                                        "PartwayCollege",
                                        ifelse(education %in% c("Bachelor's degree in college (4-year)"), "FourYearCollege",
                                               "AdvancedDegree"))),
         Education_Vals = ifelse(education == "Less than high school degree", 1,
                                 ifelse(education == "High school graduate (high school diploma or equivalent including GED)", 2,
                                        ifelse(education == "Some college but no degree", 3,
                                               ifelse(education == "Associate degree in college (2-year)", 4,
                                                      ifelse(education == "Bachelor's degree in college (4-year)", 5, 
                                                             ifelse(education == "Master's degree", 6,
                                                                    ifelse(education == "Professional degree (JD, MD)", 7,
                                                                           ifelse(education == "Doctoral degree", 8, 0)))))))),
         Income_Bins = ifelse(income %in% c("Less than $10,000", "$10,000 to $19,999", "$20,000 to $29,999", 
                                            "$30,000 to $39,999", "$40,000 to $49,999"), "<$50k",
                              ifelse(income %in% c("$50,000 to $59,999", "$60,000 to $69,999", "$70,000 to $79,999",
                                                   "$80,000 to $89,999", "$90,000 to $99,999"), "$50k-$100k", "$100k+")),
         Race_Bins = ifelse(hispanic == "Yes", "Hispanic", ifelse(race == "White", "White", 
                                                                  ifelse(race == "Black or African American", "Black",
                                                                         ifelse(race == "Asian", "Asian", "Other_POC")))),
         Religion_Bins = ifelse(religion == "No religion", "NoReligion", 
                                ifelse(religion == "Protestant", "OtherChristian",
                                       ifelse(religion == "Catholic", "Catholic", "OtherReligion")))
  ) %>%
  filter(Gender_Bins != "Unknown") %>%
  mutate(Gender_BinFactor = factor(Gender_Bins, level=c("Male", "NotMale")),
         Age_BinFactor = factor(Age_Bins, levels=c("[18,30)", "[30,40)","[40,50)", "[50,100]")),
         Education_BinFactor = factor(Education_Bins, levels=c("HighSchool", "PartwayCollege",
                                                               "FourYearCollege", "AdvancedDegree")),
         Income_BinFactor = factor(Income_Bins, levels=c("<$50k", "$50k-$100k", "$100k+")),
         Race_BinFactor = factor(Race_Bins, levels=c("White", "Hispanic", "Black", "Asian", "Other_POC")),
         Religion_BinFactor = factor(Religion_Bins, levels=c("NoReligion", "Catholic", "OtherChristian", "OtherReligion")),
         SNAP_Recipient_BinFactor = factor(SNAP_Recipient_Fac, levels=c("No","Yes")),
         Slope_BinFactor = factor(slope_factor, levels=c("equal", "low", "high", "fliplow", "fliphigh")),
         Political_BinFactor = factor(Political_Fac, levels=c("Democrat", "Republican"))
  ) %>%
  mutate(Spanish_Speaker = ifelse(spanish_lang == "", "No", spanish_lang))

user_perc_prefs_bins <- user_perc_prefs %>%
  mutate(White_BinFactor = ifelse(Race_BinFactor=='White','White','NonWhite'),
         AgeUnder40_BinFactor = ifelse(Age_BinFactor %in% c('[18,30)','[30,40)'),'Under40', 'Over40'),
         College_BinFactor = ifelse(Education_BinFactor %in% c('HighSchool', 'PartwayCollege'), 'LessThanCollege', 'College'),
         Religious_BinFactor = ifelse(Religion_BinFactor == 'NoReligion', 'NotReligious', 'Religious'),
         IncomeUnder50k_BinFactor = ifelse(Income_BinFactor == '<$50k', 'Under$50k', 'Over$50k')) %>%
  mutate(bobsteve_maxconv = ifelse(BobSteve_Answer == 'Maximize_Conversions',1,0),
         trolley_maxconv = ifelse(Trolley_Answer == 'Prefer_English',1,0),
         pareto_maxconv = leftmost_flag)

lowhigh_userprefs <- user_perc_prefs_bins %>% filter(slope %in% c('low', 'high'))
synth_userprefs <- user_perc_prefs_bins %>% filter(!(slope %in% c('low', 'high')))

#################################################################################################
################################# GET CELL LEVEL DATA FOR HIGH/LOW ESTIMATES ####################
#################################################################################################

rawbin.data <- expand.grid(Slope_BinFactorhigh = unique(c(0,1)),
                           Political_BinFactorRepublican = unique(c(0,1)),
                           Gender_BinFactorNotMale = unique(c(0,1)),
                           Race_BinFactor = unique(lowhigh_userprefs$Race_BinFactor),
                           Religion_BinFactor = unique(lowhigh_userprefs$Religion_BinFactor),
                           Age_BinFactor = unique(lowhigh_userprefs$Age_BinFactor),
                           Education_BinFactor = unique(lowhigh_userprefs$Education_BinFactor),
                           Income_BinFactor = unique(lowhigh_userprefs$Income_BinFactor)
)
rawbin.clean <- as.data.frame(rawbin.data) %>%
  mutate(Race_BinFactorHispanic = ifelse(Race_BinFactor=='Hispanic', 1, 0),
         Race_BinFactorBlack = ifelse(Race_BinFactor=='Black',1,0),
         Race_BinFactorAsian = ifelse(Race_BinFactor=='Asian',1,0),
         Race_BinFactorOther_POC = ifelse(Race_BinFactor=='Other_POC',1,0),
         Religion_BinFactorCatholic = ifelse(Religion_BinFactor=='Catholic',1,0),
         Religion_BinFactorOtherChristian = ifelse(Religion_BinFactor=='OtherChristian',1,0),
         Religion_BinFactorOtherReligion = ifelse(Religion_BinFactor=='OtherReligion',1,0)) %>%
  mutate(Age_Vals = ifelse(Age_BinFactor == "[18,30)", 24,
                           ifelse(Age_BinFactor == "[30,40)", 35,
                                  ifelse(Age_BinFactor == "[40,50)", 45,
                                         ifelse(Age_BinFactor == "[50,100]", 75, 0))))) %>%
  mutate(Education_Vals = ifelse(Education_BinFactor == "HighSchool", 1.5,
                                 ifelse(Education_BinFactor == "PartwayCollege", 3.5,
                                        ifelse(Education_BinFactor == "FourYearCollege", 5,
                                               ifelse(Education_BinFactor == "AdvancedDegree", 7, 0))))) %>%
  mutate(Income_Vals = ifelse(Income_BinFactor == "<$50k", 25000,
                              ifelse(Income_BinFactor == "$50k-$100k", 75000,
                                     ifelse(Income_BinFactor == "$100k+", 125000, 0)))) %>%
  mutate(Slope_BinFactor = ifelse(Slope_BinFactorhigh == 1, 'high', 'low'),
         Political_BinFactor = ifelse(Political_BinFactorRepublican == 1, 'Republican', 'Democrat'),
         Gender_BinFactor = ifelse(Gender_BinFactorNotMale == 1, 'NotMale', 'Male'))

ap_vote_groupsknown <- ap_vote_groups %>%
  filter(unknown_flag == 0) %>%
  filter(Political_BinFactor != "Independent")

preds_and_weights <- rawbin.clean %>% 
  left_join(ap_vote_groupsknown,
            by = c('Political_BinFactor', 'Gender_BinFactor', 'Race_BinFactor', 'Age_BinFactor',
                   'Education_BinFactor', 'Income_BinFactor', 'Religion_BinFactor'))
preds_and_weights$sum_final_natl_weights[is.na(preds_and_weights$sum_final_natl_weights)] <- 0
subset.cols <- preds_and_weights[c('Slope_BinFactor', 'Political_BinFactor', 'sum_final_natl_weights')]

###############################################################################################################
################################# GET CELL LEVEL DATA FOR EQUAL/FLIPLOW/FLIPHIGH ESTIMATES ####################
###############################################################################################################

synthbin.data <- expand.grid(Slope_BinFactor = unique(synth_userprefs$Slope_BinFactor),
                           Political_BinFactorRepublican = unique(c(0,1)),
                           Gender_BinFactorNotMale = unique(c(0,1)),
                           Race_BinFactor = unique(synth_userprefs$Race_BinFactor),
                           Religion_BinFactor = unique(synth_userprefs$Religion_BinFactor),
                           Age_BinFactor = unique(synth_userprefs$Age_BinFactor),
                           Education_BinFactor = unique(synth_userprefs$Education_BinFactor),
                           Income_BinFactor = unique(synth_userprefs$Income_BinFactor)
)
synthbin.clean <- as.data.frame(synthbin.data) %>%
  mutate(Slope_BinFactorfliplow = ifelse(Slope_BinFactor=='fliplow', 1, 0),
         Slope_BinFactorfliphigh = ifelse(Slope_BinFactor=='fliphigh',1,0),
         Race_BinFactorHispanic = ifelse(Race_BinFactor=='Hispanic', 1, 0),
         Race_BinFactorBlack = ifelse(Race_BinFactor=='Black',1,0),
         Race_BinFactorAsian = ifelse(Race_BinFactor=='Asian',1,0),
         Race_BinFactorOther_POC = ifelse(Race_BinFactor=='Other_POC',1,0),
         Religion_BinFactorCatholic = ifelse(Religion_BinFactor=='Catholic',1,0),
         Religion_BinFactorOtherChristian = ifelse(Religion_BinFactor=='OtherChristian',1,0),
         Religion_BinFactorOtherReligion = ifelse(Religion_BinFactor=='OtherReligion',1,0)) %>%
  mutate(Age_Vals = ifelse(Age_BinFactor == "[18,30)", 24,
                           ifelse(Age_BinFactor == "[30,40)", 35,
                                  ifelse(Age_BinFactor == "[40,50)", 45,
                                         ifelse(Age_BinFactor == "[50,100]", 75, 0))))) %>%
  mutate(Education_Vals = ifelse(Education_BinFactor == "HighSchool", 1.5,
                                 ifelse(Education_BinFactor == "PartwayCollege", 3.5,
                                        ifelse(Education_BinFactor == "FourYearCollege", 5,
                                               ifelse(Education_BinFactor == "AdvancedDegree", 7, 0))))) %>%
  mutate(Income_Vals = ifelse(Income_BinFactor == "<$50k", 25000,
                              ifelse(Income_BinFactor == "$50k-$100k", 75000,
                                     ifelse(Income_BinFactor == "$100k+", 125000, 0)))) %>%
  mutate(Political_BinFactor = ifelse(Political_BinFactorRepublican == 1, 'Republican', 'Democrat'),
         Gender_BinFactor = ifelse(Gender_BinFactorNotMale == 1, 'NotMale', 'Male'))

synth_preds_and_weights <- synthbin.clean %>% 
  left_join(ap_vote_groupsknown,
            by = c('Political_BinFactor', 'Gender_BinFactor', 'Race_BinFactor', 'Age_BinFactor',
                   'Education_BinFactor', 'Income_BinFactor', 'Religion_BinFactor'))
synth_preds_and_weights$sum_final_natl_weights[is.na(synth_preds_and_weights$sum_final_natl_weights)] <- 0
synth.subset.cols <- synth_preds_and_weights[c('Slope_BinFactor', 'Political_BinFactor', 'sum_final_natl_weights')]

######################################################################################################################
################################# REGRESSIONS BY TRADE-OFF ARM (Tables 2, 3) #########################################
######################################################################################################################

pareto_combine <- glm(pareto_maxconv ~ Slope_BinFactor + Political_BinFactor + Gender_BinFactor + Race_BinFactor + Religion_BinFactor +
                        Age_Vals + Education_Vals + log(Income_Vals), 
                      data = lowhigh_userprefs, 
                      family = "binomial")
summary(pareto_combine)
print("pareto")
stargazer(pareto_combine, single.row=TRUE)

synth_pareto_combine <- glm(pareto_maxconv ~ Slope_BinFactor + Political_BinFactor + Gender_BinFactor + Race_BinFactor + Religion_BinFactor +
                              Age_Vals + Education_Vals + log(Income_Vals), 
                            data = synth_userprefs, 
                            family = "binomial")
summary(synth_pareto_combine)
print("synth_pareto")
stargazer(synth_pareto_combine, single.row=TRUE)

######################################################################################################################
################################# BOOTSTRAP PREDICTIONS FOR FIG 4 (LOW/HIGH ARMS) ####################################
######################################################################################################################

GenerateAggStats <- function(model, nsims){
  # simulate nsims times using arm (posterior from regressions above)
  sim.mlogit <- sim(model, n.sims=nsims)
  coef.sim.mlogit <- as.data.frame(coef(sim.mlogit))
  sim.ests <- as.data.frame(NULL)
  
  # loop by nsims
  for(i in (1:nsims)){
    coef.sim.mlogit.row <- slice(coef.sim.mlogit,i)
    est.y <- coef.sim.mlogit.row$`(Intercept)` + 
      coef.sim.mlogit.row$Slope_BinFactorhigh * preds_and_weights$Slope_BinFactorhigh  +
      coef.sim.mlogit.row$Political_BinFactorRepublican * preds_and_weights$Political_BinFactorRepublican +
      coef.sim.mlogit.row$Gender_BinFactorNotMale * preds_and_weights$Gender_BinFactorNotMale  +
      coef.sim.mlogit.row$Race_BinFactorHispanic * preds_and_weights$Race_BinFactorHispanic  +
      coef.sim.mlogit.row$Race_BinFactorBlack * preds_and_weights$Race_BinFactorBlack  +
      coef.sim.mlogit.row$Race_BinFactorAsian * preds_and_weights$Race_BinFactorAsian  +
      coef.sim.mlogit.row$Race_BinFactorOther_POC * preds_and_weights$Race_BinFactorOther_POC  +
      coef.sim.mlogit.row$Religion_BinFactorCatholic * preds_and_weights$Religion_BinFactorCatholic +
      coef.sim.mlogit.row$Religion_BinFactorOtherChristian * preds_and_weights$Religion_BinFactorOtherChristian  +
      coef.sim.mlogit.row$Religion_BinFactorOtherReligion * preds_and_weights$Religion_BinFactorOtherReligion  +
      coef.sim.mlogit.row$Age_Vals * preds_and_weights$Age_Vals  +
      coef.sim.mlogit.row$Education_Vals * preds_and_weights$Education_Vals  +
      coef.sim.mlogit.row$`log(Income_Vals)` * log(preds_and_weights$Income_Vals)
    est.p <- exp(est.y) / (1+exp(est.y))
    
    grouped.weight <- cbind(i, preds_and_weights, as.data.frame(est.p))
    sim.ests <- rbind(sim.ests, grouped.weight)
  }
  
  WeightGroups <- function(col, title){
    sim.ests$demographic <- sim.ests[,col]
    grouped_df <- sim.ests %>%
      group_by(i, Slope_BinFactor, demographic) %>%
      summarise(weighted_logit_pred = weighted.mean(est.p, sum_final_natl_weights)) %>%
      mutate(demographic_type = title)
    get_ci <- grouped_df %>%
      group_by(Slope_BinFactor, demographic) %>%
      summarise(n=n(),mean = mean(weighted_logit_pred),
                std = sd(weighted_logit_pred),
                se = qnorm(.975)*std/sqrt(length(weighted_logit_pred)),
                lower = mean(weighted_logit_pred) - qnorm(.975)*std,
                upper = mean(weighted_logit_pred) + qnorm(.975)*std)%>%
      mutate(group = title)
    return(get_ci)
  }
  poli <- WeightGroups("Political_BinFactor", "Party Identity")
  gender <- WeightGroups("Gender_BinFactor", "Gender")
  race <- WeightGroups("Race_BinFactor", "Race")
  age <- WeightGroups("Age_BinFactor", "Age")
  edu <- WeightGroups("Education_BinFactor", "Education")
  income <- WeightGroups("Income_BinFactor", "Income")
  religion <- WeightGroups("Religion_BinFactor", "Religion")
  
  # clean names for plotting
  est.with.ci <- rbind(poli, gender, race, age, edu, income, religion) %>%
    mutate(slope_clean = ifelse(Slope_BinFactor == "low", "Low", #"Low Trade-off\n(3 English : 1 Spanish)",
                                ifelse(Slope_BinFactor == "high", "High", "Unknown")), #"High Trade-off\n(6 English : 1 Spanish)", "Unknown")),
           slope_fac = factor(slope_clean,
                              levels = c("High", "Low"))) %>% 
    mutate(clean_demographic = ifelse(demographic == "AdvancedDegree", "Advanced Degree",
                                      ifelse(demographic == "FourYearCollege", "4-Year College",
                                             ifelse(demographic == "HighSchool", "High School",
                                                    ifelse(demographic == "PartwayCollege", "Partway College /\nAssociate's Degree",
                                                           ifelse(demographic == "[18,30)", "18-29",
                                                                  ifelse(demographic == "[30,40)", "30-39",
                                                                         ifelse(demographic == "[40,50)", "40-49",
                                                                                ifelse(demographic == "[50,100]", "50-100",
                                                                                       ifelse(demographic == "NotMale", "Female or Non-Binary",
                                                                                              ifelse(demographic == "Other_POC", "Other POC",
                                                                                                     ifelse(demographic == "NoReligion", "No Religion",
                                                                                                            ifelse(demographic == "OtherChristian", "Protestant /\n Other Christian",
                                                                                                                   ifelse(demographic == "OtherReligion", "Other Religion", demographic)))))))))))))) %>%
    mutate(demo_factor = factor(clean_demographic, levels=c("18-29", "30-39", "40-49", "50-100",
                                                            "High School", "Partway College /\nAssociate's Degree","4-Year College","Advanced Degree",
                                                            "Female or Non-Binary", "Male",
                                                            "<$50k", "$50k-$100k", "$100k+",
                                                            "Democrat", "Republican",
                                                            "Hispanic", "Black", "Other POC", "Asian", "White",
                                                            "No Religion", "Catholic", "Protestant /\n Other Christian", "Other Religion"
    )))
  
  return(list(sim.ests,est.with.ci))
}
fig4_output <- GenerateAggStats(pareto_combine, 1000)
bootstrap_ests <- fig4_output[[1]]
fig4_ci_clean <- fig4_output[[2]]

fig4_ci_plot <- ggplot(fig4_ci_clean, aes(x=demo_factor, y=mean, group=slope_fac,
                                     color=slope_fac, shape = slope_fac)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(aes(shape=slope_fac), position=position_dodge(0.3), size=6, stroke=2) +
  geom_line(size=2)+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=23),
        text=element_text(size=30),
        axis.text.y = element_text(size=30),
        plot.title=element_text(size=30, face="bold"),
        axis.ticks.length=unit(.4, "cm"),
        axis.title=element_text(size=30, face="bold")) +
  scale_y_continuous(labels = scales::percent_format(accuracy=5L), limits=c(0,1), expand = c(0,0)) +
  scale_color_discrete(name="Trade-Off") +
  scale_shape_discrete(name="Trade-Off") +
  labs(y="Proportion who prefer\nthe most efficient allocation", x="") +
  geom_hline(yintercept=.5, linetype = "dashed", size=2.5) +
  facet_grid(cols = vars(group), scales = "free")

fig4_ci_plot
fig4_ci_plot %>% ggexport(filename = paste0(plot_dir,"Fig4.png"), width=2250, height=850)

# Significant political differences: CIs outside 0
reshape_bootstrap_poli <- bootstrap_ests %>%
  dplyr::select(-Political_BinFactorRepublican, -sum_final_natl_weights, -sum_pollclose_natl_weights, -unknown_flag) %>%
  spread(Political_BinFactor, est.p) %>% 
  mutate(diff = Republican-Democrat)

poli_weights <- ap_vote_groupsknown %>% 
  mutate(sum_final_natl_weights_nona = ifelse(is.na(sum_final_natl_weights),0,sum_final_natl_weights)) %>%
  group_by(Gender_BinFactor, Race_BinFactor, Age_BinFactor, Education_BinFactor,
           Income_BinFactor, Religion_BinFactor) %>%
  summarize(sum_final_natl_weights=sum(sum_final_natl_weights_nona))

merge <- reshape_bootstrap_poli %>%
  left_join(poli_weights, by = c('Gender_BinFactor', 'Race_BinFactor', 'Age_BinFactor',
                                 'Education_BinFactor', 'Income_BinFactor', 'Religion_BinFactor')) %>%
  mutate(weight = ifelse(is.na(sum_final_natl_weights),0,sum_final_natl_weights)) 

merge %>%
  group_by(Slope_BinFactor) %>%
  summarise(n=n(),mean = weighted.mean(diff, weight),
            std = sqrt(wtd.var(diff, weight)),
            lower = weighted.mean(diff, weight) - qnorm(.975)*std,
            upper = weighted.mean(diff, weight) + qnorm(.975)*std)

######################################################################################################################
################################# BOOTSTRAP PREDICTIONS FOR APPENDIX FIG 8 (EQUAL/FLIPLOW/FLIPHIGH) ##################
######################################################################################################################

SynthGenerateAggStats <- function(model, nsims){
  # simulate nsims times
  sim.mlogit <- sim(model, n.sims=nsims)
  coef.sim.mlogit <- as.data.frame(coef(sim.mlogit))
  sim.ests <- as.data.frame(NULL)
  
  # loop by nsims
  for(i in (1:nsims)){
    coef.sim.mlogit.row <- slice(coef.sim.mlogit,i)
    est.y <- coef.sim.mlogit.row$`(Intercept)` + 
      coef.sim.mlogit.row$Slope_BinFactorfliplow * synth_preds_and_weights$Slope_BinFactorfliplow  +
      coef.sim.mlogit.row$Slope_BinFactorfliphigh * synth_preds_and_weights$Slope_BinFactorfliphigh  +
      coef.sim.mlogit.row$Political_BinFactorRepublican * synth_preds_and_weights$Political_BinFactorRepublican +
      coef.sim.mlogit.row$Gender_BinFactorNotMale * synth_preds_and_weights$Gender_BinFactorNotMale  +
      coef.sim.mlogit.row$Race_BinFactorHispanic * synth_preds_and_weights$Race_BinFactorHispanic  +
      coef.sim.mlogit.row$Race_BinFactorBlack * synth_preds_and_weights$Race_BinFactorBlack  +
      coef.sim.mlogit.row$Race_BinFactorAsian * synth_preds_and_weights$Race_BinFactorAsian  +
      coef.sim.mlogit.row$Race_BinFactorOther_POC * synth_preds_and_weights$Race_BinFactorOther_POC  +
      coef.sim.mlogit.row$Religion_BinFactorCatholic * synth_preds_and_weights$Religion_BinFactorCatholic +
      coef.sim.mlogit.row$Religion_BinFactorOtherChristian * synth_preds_and_weights$Religion_BinFactorOtherChristian  +
      coef.sim.mlogit.row$Religion_BinFactorOtherReligion * synth_preds_and_weights$Religion_BinFactorOtherReligion  +
      coef.sim.mlogit.row$Age_Vals * synth_preds_and_weights$Age_Vals  +
      coef.sim.mlogit.row$Education_Vals * synth_preds_and_weights$Education_Vals  +
      coef.sim.mlogit.row$`log(Income_Vals)` * log(synth_preds_and_weights$Income_Vals)
    est.p <- exp(est.y) / (1+exp(est.y))
    
    grouped.weight <- cbind(i, synth_preds_and_weights, as.data.frame(est.p))
    sim.ests <- rbind(sim.ests, grouped.weight)
  }
  
  WeightGroups <- function(col, title){
    sim.ests$demographic <- sim.ests[,col]
    grouped_df <- sim.ests %>%
      group_by(i, Slope_BinFactor, demographic) %>%
      summarise(weighted_logit_pred = weighted.mean(est.p, sum_final_natl_weights)) %>%
      mutate(demographic_type = title)
    get_ci <- grouped_df %>%
      group_by(Slope_BinFactor, demographic) %>%
      summarise(n=n(),mean = mean(weighted_logit_pred),
                std = sd(weighted_logit_pred),
                se = qnorm(.975)*std/sqrt(length(weighted_logit_pred)),
                lower = mean(weighted_logit_pred) - qnorm(.975)*std,
                upper = mean(weighted_logit_pred) + qnorm(.975)*std)%>%
      mutate(group = title)
    return(get_ci)
  }
  poli <- WeightGroups("Political_BinFactor", "Party Identity")
  gender <- WeightGroups("Gender_BinFactor", "Gender")
  race <- WeightGroups("Race_BinFactor", "Race")
  age <- WeightGroups("Age_BinFactor", "Age")
  edu <- WeightGroups("Education_BinFactor", "Education")
  income <- WeightGroups("Income_BinFactor", "Income")
  religion <- WeightGroups("Religion_BinFactor", "Religion")
  
  est.with.ci <- rbind(poli, gender, race, age, edu, income, religion) %>%
    mutate(slope_clean = ifelse(Slope_BinFactor == "equal", "Equal",
                                ifelse(Slope_BinFactor == "fliplow", "Flipped Low",
                                       ifelse(Slope_BinFactor == "fliphigh", "Flipped High", "Unknown"))),
           slope_fac = factor(slope_clean,
                              levels = c("Equal", "Flipped Low", "Flipped High"))) %>% 
    mutate(clean_demographic = ifelse(demographic == "AdvancedDegree", "Advanced Degree",
                                      ifelse(demographic == "FourYearCollege", "4-Year College",
                                             ifelse(demographic == "HighSchool", "High School",
                                                    ifelse(demographic == "PartwayCollege", "Partway College /\nAssociate's Degree",
                                                           ifelse(demographic == "[18,30)", "18-29",
                                                                  ifelse(demographic == "[30,40)", "30-39",
                                                                         ifelse(demographic == "[40,50)", "40-49",
                                                                                ifelse(demographic == "[50,100]", "50-100",
                                                                                       ifelse(demographic == "NotMale", "Female or Non-Binary",
                                                                                              ifelse(demographic == "Other_POC", "Other POC",
                                                                                                     ifelse(demographic == "NoReligion", "No Religion",
                                                                                                            ifelse(demographic == "OtherChristian", "Protestant /\n Other Christian",
                                                                                                                   ifelse(demographic == "OtherReligion", "Other Religion", demographic)))))))))))))) %>%
    mutate(demo_factor = factor(clean_demographic, levels=c("18-29", "30-39", "40-49", "50-100",
                                                            "High School", "Partway College /\nAssociate's Degree","4-Year College","Advanced Degree",
                                                            "Female or Non-Binary", "Male",
                                                            "<$50k", "$50k-$100k", "$100k+",
                                                            "Democrat", "Republican",
                                                            "Hispanic", "Black", "Other POC", "Asian", "White",
                                                            "No Religion", "Catholic", "Protestant /\n Other Christian", "Other Religion"
    )))
  
  return(list(sim.ests,est.with.ci))
}

synth_output <- SynthGenerateAggStats(synth_pareto_combine, 1000)

fig4supp_ci_clean <- synth_output[[2]]
fig4supp_ci_plot <- ggplot(fig4supp_ci_clean, aes(x=demo_factor, y=mean, group=slope_fac,
                                          color=slope_fac, shape = slope_fac)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(aes(shape=slope_fac), position=position_dodge(0.3), size=6, stroke=2) +
  geom_line(size=2)+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=23),
        text=element_text(size=30),
        axis.text.y = element_text(size=30),
        plot.title=element_text(size=30, face="bold"),
        axis.ticks.length=unit(.4, "cm"),
        axis.title=element_text(size=30, face="bold")) +
  scale_y_continuous(labels = scales::percent_format(accuracy=5L), limits=c(0,1), expand = c(0,0)) +
  scale_color_discrete(name="Trade-Off") +
  scale_shape_discrete(name="Trade-Off") +
  labs(y="Proportion who prefer \nthe least efficient allocation\n(maximizing English conversions)", x="") +
  geom_hline(yintercept=.5, linetype = "dashed", size=2.5) +
  facet_grid(cols = vars(group), scales = "free")

fig4supp_ci_plot
fig4supp_ci_plot %>% ggexport(filename = paste0(plot_dir,"SuppFig8.png"), width=2250, height=850)


#############################################################################################################
################################# Compare Question Types by Party Identity ##################################
################################# (Regressions for Figs 6 and Supp Fig 9) ###################################
#############################################################################################################

bobsteve_combine <- glm(bobsteve_maxconv ~ Slope_BinFactor + Political_BinFactor + Gender_BinFactor + Race_BinFactor + Religion_BinFactor +
                          Age_Vals + Education_Vals + log(Income_Vals), 
                        data = lowhigh_userprefs, 
                        family = "binomial")

trolley_combine <- glm(trolley_maxconv ~ Slope_BinFactor + Political_BinFactor + Gender_BinFactor + Race_BinFactor + Religion_BinFactor +
                         Age_Vals + Education_Vals + log(Income_Vals), 
                       data = lowhigh_userprefs, 
                       family = "binomial")

synth_bobsteve_combine <- glm(bobsteve_maxconv ~ Slope_BinFactor + Political_BinFactor + Gender_BinFactor + Race_BinFactor + Religion_BinFactor +
                                Age_Vals + Education_Vals + log(Income_Vals), 
                              data = synth_userprefs, 
                              family = "binomial")

synth_trolley_combine <- glm(trolley_maxconv ~ Slope_BinFactor + Political_BinFactor + Gender_BinFactor + Race_BinFactor + Religion_BinFactor +
                               Age_Vals + Education_Vals + log(Income_Vals), 
                             data = synth_userprefs, 
                             family = "binomial")

######################################################################################################################
################################# BOOTSTRAP PREDICTIONS FOR CIs (LOW/HIGH ARMS) ######################################
######################################################################################################################

GenerateAggStatsQuestionType <- function(model, nsims, cleanname){
  # simulate nsims times
  sim.mlogit <- sim(model, n.sims=nsims)
  coef.sim.mlogit <- as.data.frame(coef(sim.mlogit))
  grouped.df <- as.data.frame(NULL)
  
  # loop by nsims
  for(i in (1:nsims)){
    coef.sim.mlogit.row <- slice(coef.sim.mlogit,i)
    est.y <- coef.sim.mlogit.row$`(Intercept)` + 
      coef.sim.mlogit.row$Slope_BinFactorhigh * preds_and_weights$Slope_BinFactorhigh  +
      coef.sim.mlogit.row$Political_BinFactorRepublican * preds_and_weights$Political_BinFactorRepublican +
      coef.sim.mlogit.row$Gender_BinFactorNotMale * preds_and_weights$Gender_BinFactorNotMale  +
      coef.sim.mlogit.row$Race_BinFactorHispanic * preds_and_weights$Race_BinFactorHispanic  +
      coef.sim.mlogit.row$Race_BinFactorBlack * preds_and_weights$Race_BinFactorBlack  +
      coef.sim.mlogit.row$Race_BinFactorAsian * preds_and_weights$Race_BinFactorAsian  +
      coef.sim.mlogit.row$Race_BinFactorOther_POC * preds_and_weights$Race_BinFactorOther_POC  +
      coef.sim.mlogit.row$Religion_BinFactorCatholic * preds_and_weights$Religion_BinFactorCatholic +
      coef.sim.mlogit.row$Religion_BinFactorOtherChristian * preds_and_weights$Religion_BinFactorOtherChristian  +
      coef.sim.mlogit.row$Religion_BinFactorOtherReligion * preds_and_weights$Religion_BinFactorOtherReligion  +
      coef.sim.mlogit.row$Age_Vals * preds_and_weights$Age_Vals  +
      coef.sim.mlogit.row$Education_Vals * preds_and_weights$Education_Vals  +
      coef.sim.mlogit.row$`log(Income_Vals)` * log(preds_and_weights$Income_Vals)
    est.p <- exp(est.y) / (1+exp(est.y))
    
    grouped.weight <- cbind(subset.cols, est.p) %>%
      group_by(Slope_BinFactor, Political_BinFactor) %>%
      summarise(weighted_logit_pred = weighted.mean(est.p, sum_final_natl_weights)) %>%
      mutate(iter=i)
    grouped.df <- rbind(grouped.df, grouped.weight)
    preds_and_weights <- cbind(preds_and_weights, as.data.frame(est.p))
  }
  
  out.df <- grouped.df %>%
    group_by(Slope_BinFactor, Political_BinFactor) %>%
    summarise(n=n(),mean = mean(weighted_logit_pred),
              std = sd(weighted_logit_pred),
              se = qnorm(.975)*std/sqrt(length(weighted_logit_pred)),
              lower = mean(weighted_logit_pred) - qnorm(.975)*std,
              upper = mean(weighted_logit_pred) + qnorm(.975)*std) %>%
    mutate(Question = cleanname)
  
  return(out.df)
}

pareto.out <- GenerateAggStatsQuestionType(pareto_combine, 1000, "Pairwise\nComparison")
bobsteve.out <- GenerateAggStatsQuestionType(bobsteve_combine, 1000, "Ideology")
trolley.out <- GenerateAggStatsQuestionType(trolley_combine, 1000, "Trolley")

# stack weighted aggregate results
weighted.avg.qs <- rbind(pareto.out, bobsteve.out, trolley.out) %>%
  mutate(slope_clean = ifelse(Slope_BinFactor=='high', 'High', 'Low'))

######################################################################################################################
################################# BOOTSTRAP PREDICTIONS FOR CIs (SYNTH ARMS) #########################################
######################################################################################################################

SynthGenerateAggStatsQuestionType <- function(model, nsims, cleanname){
  # simulate nsims times
  sim.mlogit <- sim(model, n.sims=nsims)
  coef.sim.mlogit <- as.data.frame(coef(sim.mlogit))
  grouped.df <- as.data.frame(NULL)
  
  # loop by nsims
  for(i in (1:nsims)){
    coef.sim.mlogit.row <- slice(coef.sim.mlogit,i)
    est.y <- coef.sim.mlogit.row$`(Intercept)` + 
      coef.sim.mlogit.row$Slope_BinFactorfliplow * synth_preds_and_weights$Slope_BinFactorfliplow  +
      coef.sim.mlogit.row$Slope_BinFactorfliphigh * synth_preds_and_weights$Slope_BinFactorfliphigh  +
      coef.sim.mlogit.row$Political_BinFactorRepublican * synth_preds_and_weights$Political_BinFactorRepublican +
      coef.sim.mlogit.row$Gender_BinFactorNotMale * synth_preds_and_weights$Gender_BinFactorNotMale  +
      coef.sim.mlogit.row$Race_BinFactorHispanic * synth_preds_and_weights$Race_BinFactorHispanic  +
      coef.sim.mlogit.row$Race_BinFactorBlack * synth_preds_and_weights$Race_BinFactorBlack  +
      coef.sim.mlogit.row$Race_BinFactorAsian * synth_preds_and_weights$Race_BinFactorAsian  +
      coef.sim.mlogit.row$Race_BinFactorOther_POC * synth_preds_and_weights$Race_BinFactorOther_POC  +
      coef.sim.mlogit.row$Religion_BinFactorCatholic * synth_preds_and_weights$Religion_BinFactorCatholic +
      coef.sim.mlogit.row$Religion_BinFactorOtherChristian * synth_preds_and_weights$Religion_BinFactorOtherChristian  +
      coef.sim.mlogit.row$Religion_BinFactorOtherReligion * synth_preds_and_weights$Religion_BinFactorOtherReligion  +
      coef.sim.mlogit.row$Age_Vals * synth_preds_and_weights$Age_Vals  +
      coef.sim.mlogit.row$Education_Vals * synth_preds_and_weights$Education_Vals  +
      coef.sim.mlogit.row$`log(Income_Vals)` * log(synth_preds_and_weights$Income_Vals)
    est.p <- exp(est.y) / (1+exp(est.y))
    
    grouped.weight <- cbind(synth.subset.cols, est.p) %>%
      group_by(Slope_BinFactor, Political_BinFactor) %>%
      summarise(weighted_logit_pred = weighted.mean(est.p, sum_final_natl_weights)) %>%
      mutate(iter=i)
    grouped.df <- rbind(grouped.df, grouped.weight)
    synth_preds_and_weights <- cbind(synth_preds_and_weights, as.data.frame(est.p))
  }
  
  out.df <- grouped.df %>%
    group_by(Slope_BinFactor, Political_BinFactor) %>%
    summarise(n=n(),mean = mean(weighted_logit_pred),
              std = sd(weighted_logit_pred),
              se = qnorm(.975)*std/sqrt(length(weighted_logit_pred)),
              lower = mean(weighted_logit_pred) - qnorm(.975)*std,
              upper = mean(weighted_logit_pred) + qnorm(.975)*std) %>%
    mutate(Question = cleanname)
  
  return(out.df)
}

synth.pareto.out <- SynthGenerateAggStatsQuestionType(synth_pareto_combine, 1000, "Pairwise\nComparison")
synth.bobsteve.out <- SynthGenerateAggStatsQuestionType(synth_bobsteve_combine, 1000, "Ideology")
synth.trolley.out <- SynthGenerateAggStatsQuestionType(synth_trolley_combine, 1000, "Trolley")

# stack weighted aggregate results
synth.weighted.avg.qs <- rbind(synth.pareto.out, synth.bobsteve.out, synth.trolley.out) %>%
  mutate(slope_clean = ifelse(Slope_BinFactor=='equal', 'Equal', 
                              ifelse(Slope_BinFactor=='fliplow', 'Flipped Low', 'Flipped High')))

###########################################################################################
################################# PLOT FIGURE 6 ###########################################
###########################################################################################

pairwise <- ggplot(weighted.avg.qs %>% filter(Question=='Pairwise\nComparison') %>%
                     mutate(
                       slope_fac = factor(slope_clean, level=c("High","Low")),
                       poli_ordering = factor(Political_BinFactor, level=c('Republican','Democrat'))), 
                   aes(x=Question, y=mean, group=slope_fac, color=poli_ordering, fill=slope_fac,
                       shape=slope_fac)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(aes(shape=slope_fac), position=position_dodge(0.3), size=6, stroke=2) +
  scale_shape_manual(values=c(1,19)) +
  scale_fill_discrete(guide='none') +
  scale_color_manual(values=c('Republican'='red',
                              'Democrat'='blue')) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
  theme(text=element_text(size=25),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        plot.title=element_text(size=30),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Proportion preferring \nthe most efficient allocation",
       x="", color="Party Identity", shape="Trade-off") +
  geom_hline(yintercept=0.5, linetype="dashed", size = 2) +
  theme(aspect.ratio=1/3)+
  theme(plot.margin = margin(1,1,1,1, "cm"))  + guides(shape = guide_legend(reverse = TRUE)) + coord_flip()
pairwise

ideology <- ggplot(weighted.avg.qs %>% filter(Question=='Ideology') %>%
                     mutate(
                       slope_fac = factor(slope_clean, level=c("High","Low")),
                       poli_ordering = factor(Political_BinFactor, level=c('Republican','Democrat'))), 
                   aes(x=Question, y=mean, group=slope_fac, color=poli_ordering, fill=slope_fac,
                       shape=slope_fac)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(aes(shape=slope_fac), position=position_dodge(0.3), size=6, stroke=2) +
  scale_shape_manual(values=c(1,19)) +
  scale_fill_discrete(guide='none') +
  scale_color_manual(values=c('Republican'='red',
                              'Democrat'='blue')) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
  theme(text=element_text(size=25),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        plot.title=element_text(size=30),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Proportion preferring the most efficient allocation \nover demographic parity",
       x="", color="Party Identity", shape="Trade-off") +
  geom_hline(yintercept=0.5, linetype="dashed", size = 2) +
  theme(aspect.ratio=1/3)+
  theme(plot.margin = margin(1,1,1,1, "cm"))  + guides(shape = guide_legend(reverse = TRUE)) + coord_flip()

trolley <- ggplot(weighted.avg.qs %>% filter(Question=='Trolley') %>%
                    mutate(
                      slope_fac = factor(slope_clean, level=c("High","Low")),
                      poli_ordering = factor(Political_BinFactor, level=c('Republican','Democrat'))), 
                  aes(x=Question, y=mean, group=slope_fac, color=poli_ordering, fill=slope_fac,
                      shape=slope_fac)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(aes(shape=slope_fac), position=position_dodge(0.3), size=6, stroke=2) +
  scale_shape_manual(values=c(1,19)) +
  scale_fill_discrete(guide='none') +
  scale_color_manual(values=c('Republican'='red',
                              'Democrat'='blue')) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
  theme(text=element_text(size=25),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        plot.title=element_text(size=30),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Proportion preferring efficient allocation \non the margin",
       x="", color="Party Identity", shape="Trade-off") +
  geom_hline(yintercept=0.5, linetype="dashed", size = 2) +
  theme(aspect.ratio=1/3)+
  theme(plot.margin = margin(1,1,1,1, "cm")) + guides(shape = guide_legend(reverse = TRUE)) + coord_flip()

fig6 <- ggarrange(pairwise, ideology, trolley,
                  labels = NULL,
                  ncol = 1, nrow = 3,
                  common.legend = TRUE, legend = "right",
                  align = "hv") 
fig6
fig6 %>% ggexport(filename = paste0(plot_dir,"Fig6.png"), width=1500, height=1200)

####################################################################################################
################################# PLOT APPENDIX FIGURE 9 ###########################################
####################################################################################################

synth.pairwise <- ggplot(synth.weighted.avg.qs %>% filter(Question=='Pairwise\nComparison') %>%
                           mutate(
                             slope_fac = factor(slope_clean, level=c("Equal", "Flipped Low", "Flipped High")),
                             poli_ordering = factor(Political_BinFactor, level=c('Republican','Democrat'))), 
                         aes(x=Question, y=mean, group=slope_fac, color=poli_ordering, fill=slope_fac,
                             shape=slope_fac)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(aes(shape=slope_fac), position=position_dodge(0.3), size=6, stroke=2) +
  scale_shape_manual(values=c(9,19,1)) +
  scale_fill_discrete(guide='none') +
  scale_color_manual(values=c('Republican'='red',
                              'Democrat'='blue')) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
  theme(text=element_text(size=25),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        plot.title=element_text(size=30),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Proportion preferring \nthe allocation with most English speakers",
       x="", color="Party Identity", shape="Trade-off") +
  geom_hline(yintercept=0.5, linetype="dashed", size = 2) +
  theme(aspect.ratio=1/3)+
  theme(plot.margin = margin(1,1,1,1, "cm")) + guides(shape = guide_legend(reverse = TRUE))  + coord_flip()

synth.ideology <- ggplot(synth.weighted.avg.qs %>% filter(Question=='Ideology') %>%
                           mutate(
                             slope_fac = factor(slope_clean, level=c("Equal", "Flipped Low", "Flipped High")),
                             poli_ordering = factor(Political_BinFactor, level=c('Republican','Democrat'))), 
                         aes(x=Question, y=mean, group=slope_fac, color=poli_ordering, fill=slope_fac,
                             shape=slope_fac)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(aes(shape=slope_fac), position=position_dodge(0.3), size=6, stroke=2) +
  scale_shape_manual(values=c(9,19,1)) +
  scale_fill_discrete(guide='none') +
  scale_color_manual(values=c('Republican'='red',
                              'Democrat'='blue')) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
  theme(text=element_text(size=25),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        plot.title=element_text(size=30),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Proportion preferring the most efficient allocation \nover demographic parity",
       x="", color="Party Identity", shape="Trade-off") +
  geom_hline(yintercept=0.5, linetype="dashed", size = 2) +
  theme(aspect.ratio=1/3)+
  theme(plot.margin = margin(1,1,1,1, "cm")) + guides(shape = guide_legend(reverse = TRUE))  + coord_flip()

synth.trolley <- ggplot(synth.weighted.avg.qs %>% filter(Question=='Trolley') %>%
                          mutate(
                            slope_fac = factor(slope_clean, level=c("Equal", "Flipped Low", "Flipped High")),
                            poli_ordering = factor(Political_BinFactor, level=c('Republican','Democrat'))), 
                        aes(x=Question, y=mean, group=slope_fac, color=poli_ordering, fill=slope_fac,
                            shape=slope_fac)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(aes(shape=slope_fac), position=position_dodge(0.3), size=6, stroke=2) +
  scale_shape_manual(values=c(9,19,1)) +
  scale_fill_discrete(guide='none') +
  scale_color_manual(values=c('Republican'='red',
                              'Democrat'='blue')) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
  theme(text=element_text(size=25),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        plot.title=element_text(size=30),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Proportion preferring least efficient allocation \non the margin",
       x="", color="Party Identity", shape="Trade-off") +
  geom_hline(yintercept=0.5, linetype="dashed", size = 2) +
  theme(aspect.ratio=1/3)+
  theme(plot.margin = margin(1,1,1,1, "cm")) + guides(shape = guide_legend(reverse = TRUE)) + coord_flip()

synth.fig9 <- ggarrange(synth.pairwise, synth.ideology, synth.trolley,
                        labels = NULL,
                        ncol = 1, nrow = 3,
                        common.legend = TRUE, legend = "right",
                        align = "hv") 
synth.fig9
synth.fig9 %>% ggexport(filename = paste0(plot_dir,"SuppFig9.png"), width=1500, height=1200)
