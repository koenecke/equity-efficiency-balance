library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(scales)
library(arm)

# Import data
data_dir <- '~/data/'
plot_dir <- '~/plots/'

# Private Prolific survey data (subset to respondents passing attention checks)
prolific_df_clean <- read.csv(paste0(data_dir, 'prolific_data.csv')) 

# Public 2018 AP VoteCast data available at https://doi.org/10.3886/E109687V2
ap_vote_groups <- read.csv(paste0(data_dir, 'ap_vote_weights.csv'))

#################################################################################################################
#################### Clean Survey Data (same as Fig4_6_8_9_MultilevelRegPoststratification.R)####################
################################# now extracting pairwise win comparisons #######################################
#################################################################################################################

# Extract comparisons presented to each survey taker
prolific_df <- prolific_df_clean %>%
  mutate(Gender_Bins = ifelse(gender %in% c("Prefer not to state"), "Unknown", 
                              ifelse(gender %in% c("Female", "Non-binary"), "NotMale", gender)),
         Age_Vals = ifelse(age==5, 50, ifelse(age=="Thirty four", 34, ifelse(age=="2domdom", 20, as.numeric(age)))),
         Age_Bins = cut(Age_Vals, breaks = c(18, 30, 40, 50, 100), include.lowest=TRUE, right=FALSE), #seq(10, 80, 10))
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
  mutate(Spanish_Speaker = ifelse(spanish_lang == "", "No", spanish_lang)) %>%
  mutate(SpanishSpeakingHispanic = ifelse(Spanish_Speaker=='Yes' & Hispanic_Bin=='Yes',
                                          'Yes','No')) %>%
  mutate(SpanishSNAP = ifelse(SpanishSpeakingHispanic=='Yes' & welfare=='Yes',
                              'Yes','No'))

ComparisonCleaning <- function(df, slope_val){
  
  cols <- paste0(slope_val, "_pair")
  
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
    dplyr::select(-c('paren1', 'percent1', 'paren_idx1', 'perc_idx1', 'paren2', 'percent2', 'paren_idx2', 'perc_idx2'))
  
  option_level_nums <- df %>%
    filter(slope == slope_val) %>%
    dplyr::select(c('ResponseId', contains(cols))) %>%
    dplyr::select(c('ResponseId', !contains("_DO"))) %>%
    gather(question, selection, c(2:ncol(.)), factor_key=FALSE) %>%
    mutate(question = paste0(question,"_DO")) %>%
    left_join(two_choices, by=c('ResponseId', 'question')) %>%
    filter(!is.na(selection) & selection != "") %>%
    gather(option_choice, options, option1:option2, factor_key=FALSE) %>%
    mutate(win = 1*(selection==options)) %>%
    mutate(paren = str_locate_all(options, pattern ='\\(+'),
           percent = str_locate_all(options, pattern ='\\%+'),
           paren_idx = as.numeric(substr(as.character(paren),3,4)),
           perc_idx = as.numeric(substr(as.character(percent),3,4)),
           total = as.numeric(substr(options, 1, 3)),
           spanish_perc = as.numeric(substr(options, paren_idx+1, perc_idx-1)),
           spanish = round(spanish_perc*total/100, 0),
           english = total - spanish) %>%
    dplyr::select(-c('paren', 'percent', 'paren_idx', 'perc_idx'))
  
  # get plain options list
  options <- option_level_nums %>%
    distinct(options, total, spanish_perc, spanish, english) %>%
    arrange(total, english) %>%
    mutate(option_nums = paste0('Option_',seq(1:6)))
  
  return(list(question_level_nums, option_level_nums, options))
}
fliphigh_user_selections <- ComparisonCleaning(prolific_df, 'fliphigh')
fliplow_user_selections <- ComparisonCleaning(prolific_df, 'fliplow')
equal_user_selections <- ComparisonCleaning(prolific_df, 'equal')
low_user_selections <- ComparisonCleaning(prolific_df, 'low')
high_user_selections <- ComparisonCleaning(prolific_df, 'high')

# confirm # options is correct
print(
  nrow(as.data.frame(low_user_selections[3]))==6 &
    nrow(as.data.frame(high_user_selections[3]))==6 &
    nrow(as.data.frame(equal_user_selections[3]))==6 &
    nrow(as.data.frame(fliplow_user_selections[3]))==6 &
    nrow(as.data.frame(fliphigh_user_selections[3]))==6)

# merge top preferred percentages back with demographic info
user_perc_prefs <- prolific_df %>%
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
  mutate(Spanish_Speaker = ifelse(spanish_lang == "", "No", spanish_lang)) %>%
  mutate(White_BinFactor = ifelse(Race_BinFactor=='White','White','NonWhite'),
         AgeUnder40_BinFactor = ifelse(Age_BinFactor %in% c('[18,30)','[30,40)'),'Under40', 'Over40'),
         College_BinFactor = ifelse(Education_BinFactor %in% c('HighSchool', 'PartwayCollege'), 'LessThanCollege', 'College'),
         Religious_BinFactor = ifelse(Religion_BinFactor == 'NoReligion', 'NotReligious', 'Religious'),
         IncomeUnder50k_BinFactor = ifelse(Income_BinFactor == '<$50k', 'Under$50k', 'Over$50k')) %>%
  mutate(bobsteve_maxconv = ifelse(BobSteve_Answer == 'Maximize_Conversions',1,0),
         trolley_maxconv = ifelse(Trolley_Answer == 'Prefer_English',1,0))

# subset to low/high trade-off arms
lowhigh_userprefs <- user_perc_prefs %>% filter(slope %in% c('low', 'high'))

# count wins
CleanWins <- function(df, plt_title){
  question_level <- as.data.frame(df[1][[1]]) %>%
    left_join(prolific_df, by='ResponseId')
  # confirm wins counted correctly
  print(sum(question_level$win1 + question_level$win2) == nrow(question_level))
  
  option_level <- as.data.frame(df[2][[1]])
  # confirm option wins counted correctly
  print(nrow(option_level)==2*nrow(question_level))
  print(sum(option_level$win)==15*n_distinct(question_level$ResponseId))
  
  # calculate overall win rate by demographic; plot across demographics (e.g. politics)
  option_winrate <- option_level %>%
    left_join(prolific_df, by='ResponseId')
}
clean_high_wins <- CleanWins(high_user_selections, "High Trade-off (6 English: 1 Spanish)")
clean_low_wins <- CleanWins(low_user_selections, "Low Trade-off (3 English: 1 Spanish)")
clean_equal_wins <- CleanWins(equal_user_selections, "Equal Trade-off (1 English: 1 Spanish)")
clean_fliplow_wins <- CleanWins(fliplow_user_selections, "Flipped Low Trade-off (1 English: 3 Spanish)")
clean_fliphigh_wins <- CleanWins(fliphigh_user_selections, "Flipped High Trade-off (1 English: 6 Spanish)")

# Head-to-head comparison of demographic parity vs. max conversions
high_comp1 <- as.data.frame(high_user_selections[1][[1]]) %>% 
  filter(spanish_perc1 == high_user_selections[[3]]$spanish_perc[3] &
         spanish_perc2 == high_user_selections[[3]]$spanish_perc[6]) %>%
  mutate(demowin = win1)
high_comp2 <- as.data.frame(high_user_selections[1][[1]]) %>% 
  filter(spanish_perc2 == high_user_selections[[3]]$spanish_perc[3] &
         spanish_perc1 == high_user_selections[[3]]$spanish_perc[6]) %>%
  mutate(demowin = win2)
stackdemowin <- rbind(high_comp1, high_comp2) %>%
  left_join(prolific_df_clean, by=c("ResponseId"))
stackdemowin %>% group_by(political_arm) %>% summarise(1-mean(demowin)) # compare to ideological results

##########################################################################
#################### Clean and Poststratify Win Rates ####################
##########################################################################

CleanBinaryCols <- function(df){
  clean.df <- as.data.frame(df) %>%
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
                                           ifelse(Age_BinFactor == "[50,100]", 75, 0))))) %>% #75
    mutate(Education_Vals = ifelse(Education_BinFactor == "HighSchool", 1.5, #1.5
                                   ifelse(Education_BinFactor == "PartwayCollege", 3.5,
                                          ifelse(Education_BinFactor == "FourYearCollege", 5,
                                                 ifelse(Education_BinFactor == "AdvancedDegree", 7, 0))))) %>%
    mutate(Income_Vals = ifelse(Income_BinFactor == "<$50k", 25000,
                                ifelse(Income_BinFactor == "$50k-$100k", 75000,
                                       ifelse(Income_BinFactor == "$100k+", 125000, 0)))) %>%
    mutate(Political_BinFactor = ifelse(Political_BinFactorRepublican == 1, 'Republican', 'Democrat'),
           Gender_BinFactor = ifelse(Gender_BinFactorNotMale == 1, 'NotMale', 'Male'))
  
  return(clean.df)
}

# generate cells for poststratification
rawbin.data <- expand.grid(Slope_BinFactorhigh = unique(c(0,1)),
                           Political_BinFactorRepublican = unique(c(0,1)),
                           Gender_BinFactorNotMale = unique(c(0,1)),
                           Race_BinFactor = unique(lowhigh_userprefs$Race_BinFactor),
                           Religion_BinFactor = unique(lowhigh_userprefs$Religion_BinFactor),
                           Age_BinFactor = unique(lowhigh_userprefs$Age_BinFactor),
                           Education_BinFactor = unique(lowhigh_userprefs$Education_BinFactor),
                           Income_BinFactor = unique(lowhigh_userprefs$Income_BinFactor)
)

rawbin.clean <- CleanBinaryCols(rawbin.data) %>%
  mutate(Slope_BinFactor = ifelse(Slope_BinFactorhigh == 1, 'high', 'low'))

# merge with AP VoteCast data
ap_vote_groupsknown <- ap_vote_groups %>%
  filter(unknown_flag == 0) %>%
  filter(Political_BinFactor != "Independent")

preds_and_weights <- rawbin.clean %>% 
  left_join(ap_vote_groupsknown,
            by = c('Political_BinFactor', 'Gender_BinFactor', 'Race_BinFactor', 'Age_BinFactor',
                   'Education_BinFactor', 'Income_BinFactor', 'Religion_BinFactor'))
preds_and_weights$sum_final_natl_weights[is.na(preds_and_weights$sum_final_natl_weights)] <- 0
subset.cols <- preds_and_weights[c('Slope_BinFactor', 'Political_BinFactor', 'sum_final_natl_weights')]

EstimateOptionWins <- function(nsims){
  winrate_est <- data.frame()
  for(i in 1:6){
    high_num <- high_user_selections[[3]]$spanish_perc
    low_num <- low_user_selections[[3]]$spanish_perc
    high_subset <- clean_high_wins %>% filter(spanish_perc == high_num[i])
    low_subset <- clean_low_wins %>% filter(spanish_perc == low_num[i])
    stack_subset <- rbind(high_subset, low_subset)
    reg <- glm(win ~ Slope_BinFactor + Political_BinFactor + Gender_BinFactor + Race_BinFactor + Religion_BinFactor +
                 Age_Vals + Education_Vals + log(Income_Vals), 
               data = stack_subset, 
               family = "binomial")

    GenerateAggStats <- function(model, nsims){
      # simulate nsims times
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
      
      est.with.ci <- poli %>%
        mutate(slope_clean = ifelse(Slope_BinFactor == "low", "Low", #"Low Trade-off\n(3 English : 1 Spanish)",
                                    ifelse(Slope_BinFactor == "high", "High", "Unknown")), #"High Trade-off\n(6 English : 1 Spanish)", "Unknown")),
               slope_fac = factor(slope_clean,
                                  levels = c("High", "Low"))) %>% 
        mutate(demo_factor = factor(demographic, levels=c("Democrat", "Republican")))
      return(est.with.ci)
    }
    winrate_ci_clean <- GenerateAggStats(reg, nsims) %>%
      mutate(option_num=i, high_spanish_perc=high_num[i], low_spanish_perc=low_num[i])
    winrate_est <- rbind(winrate_est, winrate_ci_clean)
  }
  return(winrate_est)
}

highlow_winrates<-EstimateOptionWins(1000)

##########################################################################
#################### Plot Fig 5 ##########################################
##########################################################################

highlow_winrates_clean <- highlow_winrates %>% 
  mutate(low_to_high_spanish_perc = 7-option_num,
         spanish_perc = ifelse(Slope_BinFactor=='high', high_spanish_perc, low_spanish_perc),
         slope_plot = ifelse(Slope_BinFactor == "low", "Low Trade-off\n(3 English : 1 Spanish)",
                             "High Trade-off\n(6 English : 1 Spanish)"))

winrate_plot <- ggplot(highlow_winrates_clean, aes(x=spanish_perc, y=mean, color=demo_factor
                                                   , shape=demo_factor
)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3), size=1, fatten=.5) +
  geom_pointrange(aes(ymin=mean+std, ymax=mean-std), position=position_dodge(0.3), size=2, fatten=.5) +
  geom_point(position=position_dodge(0.3), size=3, stroke=2)+
  geom_line(size=1.5)+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"), 
        legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position='bottom',
        plot.margin=margin(10,50,10,10), 
        title=element_text(size=15),
        strip.text.x=element_text(size=15)) +
  scale_color_manual(values=c('Republican'='red',
                              'Democrat'='blue'), name="Party Identity") +
  scale_shape_manual(values=c('Republican'=16,
                              'Democrat'=17), name="Party Identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy=5L), limits=c(0,1), expand = c(0,0)) +
  labs(y="Poststratified Win Rate of Each Option", x="% Spanish Conversions in Each Surveyed Option") +
  geom_hline(yintercept=.5, linetype = "dashed", size=2.5) +
  facet_grid(cols = vars(slope_plot), scales = "free")
winrate_plot

winrate_plot %>%  ggexport(filename = paste0(plot_dir,"Fig5.png"), width=700, height=400)

############################################################################################
#################### Raw Win Rates by Group (Appendix Fig 10) ##############################
############################################################################################

AggregateWinsGrouped <- function(df, plt_title, groups, group_name){
  question_level <- as.data.frame(df[1][[1]]) %>%
    left_join(prolific_df_clean, by='ResponseId')
  # confirm wins counted correctly
  print(sum(question_level$win1 + question_level$win2) == nrow(question_level))
  
  option_level <- as.data.frame(df[2][[1]])
  # confirm option wins counted correctly
  print(nrow(option_level)==2*nrow(question_level))
  print(sum(option_level$win)==15*n_distinct(question_level$ResponseId))
  
  # get option-level df for regressions
  option_reg_df <- option_level %>%
    left_join(prolific_df_clean, by='ResponseId')
  
  # calculate win rate for each option per respondent; reshape to merge with respondent demographics
  option_user_winrate <- option_level %>%
    group_by(ResponseId, options) %>%
    dplyr::summarize(win_rate = mean(win),
                     win_num = sum(win)) %>%
    left_join(as.data.frame(df[3][[1]]), by='options') %>%
    dplyr::select(ResponseId, win_rate, option_nums) %>%
    spread(option_nums, win_rate) %>% # Option_1 = demographic parity, Option_6 = max conversions
    left_join(prolific_df_clean, by='ResponseId')
  
  question_level['group_col'] <- question_level[[groups]]
  option_reg_df['group_col'] <- option_reg_df[[groups]]
  
  # calculate win rate overall for each option w.r.t. other options
  pairwise_winners <- question_level %>%
    group_by(group_col, option1, option2) %>%
    dplyr::summarize(win_rate1 = mean(win1),
                     win_rate2 = mean(win2),
                     win_num1 = sum(win1),
                     win_num2 = sum(win2))
  # calculate overall win rate by demographic; plot across demographics (e.g. politics)
  option_winrate <- option_reg_df %>%
    group_by(group_col, options) %>%
    dplyr::summarize(win_rate = mean(win),
                     win_num = sum(win)) %>%
    arrange(group_col, desc(win_rate)) %>%
    left_join(as.data.frame(df[3]), by='options') %>%
    mutate(SpanishPercentage = spanish_perc/100)
  # plot win rates 
  
  if(groups=="political_arm"){
    slope_winplot <- ggplot(option_winrate, aes(x=SpanishPercentage, y=win_rate, group=group_col, 
                                                color=group_col, shape=group_col)) +
      geom_point(size=3)+
      geom_line() +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"), 
            legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position='top',
            plot.margin=margin(10,50,10,10), 
            title=element_text(size=15)) +
      scale_x_continuous(labels = scales::percent_format(accuracy=5L),
                         breaks = scales::pretty_breaks(n=6)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n=10), limits = c(.15,.75)) +
      scale_color_manual(values=c('Republican'='red',
                                  'Democrat'='blue'), name="Party Identity") +
      scale_shape_manual(values=c('Republican'=16,
                                  'Democrat'=17), name="Party Identity") +
      labs(y="Win Rate", x="Surveyed Options for % Spanish Conversions", color="Party Identity") +
      ggtitle(plt_title)
  }
  
  if(groups!="political_arm"){
    slope_winplot <- ggplot(option_winrate, aes(x=SpanishPercentage, y=win_rate, group=group_col, 
                                              color=group_col, shape=group_col)) +
    geom_point(size=3)+
    geom_line() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"), 
          legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position='top',
          plot.margin=margin(10,50,10,10), 
          title=element_text(size=15)) +
    scale_x_continuous(labels = scales::percent_format(accuracy=5L),
                       breaks = scales::pretty_breaks(n=6)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n=10), limits = c(.15,.75)) +
    scale_color_manual(values=c("No"='red',
                                "Yes"='blue'), name=group_name) +
    scale_shape_manual(values=c('No'=16,
                                'Yes'=17), name=group_name) +
    labs(y="Win Rate", x="Surveyed Options for % Spanish Conversions", color=group_name) +
    ggtitle(plt_title)
  }
  
  return(list(question_level,
              option_reg_df,
              option_user_winrate,
              pairwise_winners,
              option_winrate,
              slope_winplot)) # plot is the [6]th item in list
}

# Win rates comparable for Spanish-speaking and SNAP recipient subsets
poli_low_wins <- AggregateWinsGrouped(low_user_selections, "", "political_arm", "Party Identity")[[6]]
welfare_low_wins <- AggregateWinsGrouped(low_user_selections, "", "welfare", "SNAP Recipient")[[6]]
spanish_low_wins <- AggregateWinsGrouped(low_user_selections, "", "Spanish_Speaker", "Spanish Speaker")[[6]]

rawwinrates <- ggarrange(poli_low_wins + rremove("xlab"), spanish_low_wins+ rremove("xlab"), welfare_low_wins,
                         ncol = 1, nrow = 3,
                         common.legend = FALSE, legend = "right",
                         align = "hv") 
rawwinrates
rawwinrates %>%  ggexport(filename = paste0(plot_dir,"SuppFig10.png"), width=700, height=900)