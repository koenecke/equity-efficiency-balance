library(tidyverse)
library(png)
library(ggplot2)
library(waffle)
library(extrafont)
# import pictograms
font_import()
# check that Font Awesome is imported
fonts()[grep("Awesome", fonts())]
# load for mac osx
loadfonts()

################################################################################
############################ IMPORT DATA #######################################
################################################################################

data_dir <- '~/data/'
plot_dir <- '~/plots/'
pict_dir <- '~/plots/pictograms/'

maxconv_conversions <- read.csv(paste0(data_dir, 'maxconv_conversions.csv')) # GCF internal Google Ads data (not made public)
cpa_conversions <- read.csv(paste0(data_dir, 'cpa_conversions.csv')) # GCF internal Google Ads data (not made public)

################################################################################
############################ MAKE TRUE PARETO LINES ############################
################################################################################

PlotPareto <- function(df, slope_val, points, xmax, ymax){
  share_eng_campaign <- seq(0, 1, by=0.01)
  
  # make base dataset
  english_conversions <- share_eng_campaign * df$scaled_english_speakers[1] +
    (1-share_eng_campaign) * df$scaled_english_speakers[2]
  
  spanish_conversions <- share_eng_campaign * df$scaled_spanish_speakers[1] +
    (1-share_eng_campaign) * df$scaled_spanish_speakers[2]
  
  total_conversions <- english_conversions + spanish_conversions
  
  conv <- as.data.frame(cbind(spanish_conversions, english_conversions, total_conversions))
  conv$slope <- slope_val
  subset_conv <- conv[points,]
  
  # plot pareto frontier
  line_with_points <- ggplot(subset_conv, aes(x=spanish_conversions, y=total_conversions)) + 
    geom_point(size=3) +
    geom_line() +
    xlab("Spanish-Preference Enrollees") +
    ylab("Total Enrollees") +
    xlim(0,xmax) +
    ylim(0,ymax) +
    theme(aspect.ratio = 1.5) 
  
  line_with_points
  return(list(conv, subset_conv, line_with_points))
}

maxconv_plots <- PlotPareto(maxconv_conversions, "High", c(1, 67, 101), 15, 60)
cpa_plots <- PlotPareto(cpa_conversions, "Low", c(1, 101), 25, 50)

################################################################################
############################ MAKE SYNTHETIC PARETO LINES #######################
################################################################################

PlotSynthPareto <- function(df, slope_val, multiplier, points, xmax, ymax){
  share_eng_campaign <- seq(0, 1, by=0.01)
  
  # make base dataset
  if(multiplier=="Equal"){
    english_conversions <- share_eng_campaign * df$scaled_english_speakers[1] +
      (1-share_eng_campaign) * df$scaled_spanish_speakers[1]
    
    spanish_conversions <- share_eng_campaign * df$scaled_spanish_speakers[1] +
      (1-share_eng_campaign) * df$scaled_english_speakers[1]
  }
  if(multiplier=="Flip"){
    new_e1 <- df$scaled_english_speakers[1]
    new_e2 <- (df$scaled_spanish_speakers[1])^2 / df$scaled_english_speakers[2]
    new_s1 <- df$scaled_spanish_speakers[1]
    new_s2 <- (df$scaled_english_speakers[1])^2 / df$scaled_spanish_speakers[2]
    english_conversions <- share_eng_campaign * new_e1 +
      (1-share_eng_campaign) * new_e2
    
    spanish_conversions <- share_eng_campaign * new_s1 +
      (1-share_eng_campaign) * new_s2
  }
  total_conversions <- english_conversions + spanish_conversions
  
  conv <- as.data.frame(cbind(spanish_conversions, english_conversions, total_conversions))
  conv$slope <- paste0(slope_val,"_",multiplier)
  subset_conv <- conv[points,]
  
  # plot pareto frontier
  line_with_points <- ggplot(subset_conv, aes(x=spanish_conversions, y=total_conversions)) + 
    geom_point(size=3) +
    geom_line() +
    xlab("Spanish-Preference Enrollees") +
    ylab("Total Enrollees") +
    xlim(0,xmax) +
    ylim(0,ymax) +
    theme(aspect.ratio = 1.5) 
  
  line_with_points
  return(list(conv, subset_conv, line_with_points))
}

cpa_equal_plots <- PlotSynthPareto(cpa_conversions, "Low", "Equal", c(1, 101), 50, 50)
cpa_flip_plots <- PlotSynthPareto(cpa_conversions, "Low", "Flip", c(1, 101), 200, 200)
maxconv_flip_plots <- PlotSynthPareto(maxconv_conversions, "High", "Flip", c(1, 101), 300, 300)

##################################################################################################
############################ COMBINE TRUE AND SYNTHETIC ARMS  ####################################
##################################################################################################

five_arm_basedf <- rbind(maxconv_plots[[1]],
                             maxconv_flip_plots[[1]],
                             cpa_plots[[1]],
                             cpa_equal_plots[[1]],
                             cpa_flip_plots[[1]]) %>%
  mutate(spanish_perc = spanish_conversions / total_conversions,
         newslopenames = ifelse(slope=="Low_Equal", "Equal", slope),
         slope_factor = factor(newslopenames, levels=c('High_Flip', 'Low_Flip', 'Equal', 'Low', 'High')),
         int_spanish_convs = floor(spanish_conversions),
         int_english_convs = floor(english_conversions),
         int_total_convs = int_spanish_convs + int_english_convs,
         int_spanish_perc = round(100*int_spanish_convs/int_total_convs))

######################################################################################################################
############################ ROUNDING AND TRUNCATING FOR PAIRWISE COMPARISON OPTIONS #################################
######################################################################################################################

flip_round <- five_arm_basedf %>%
  filter(slope_factor %in% c("Equal", "Low_Flip", "High_Flip")) %>%
  mutate(int_spanish_convs = floor(spanish_conversions),
         int_english_convs = floor(english_conversions),
         int_total_convs = int_spanish_convs + int_english_convs,
         int_spanish_perc = round(100*int_spanish_convs/int_total_convs)) %>%
  group_by(slope_factor, int_english_convs) %>%
  slice(which.max(spanish_conversions))

true_round <- five_arm_basedf %>%
  filter(slope_factor %in% c("Low", "High")) %>%
  mutate(int_spanish_convs = floor(spanish_conversions),
         int_english_convs = floor(english_conversions),
         int_total_convs = int_spanish_convs + int_english_convs,
         int_spanish_perc = round(100*int_spanish_convs/int_total_convs)) %>%
  group_by(slope_factor, int_spanish_convs) %>%
  slice(which.max(english_conversions))

round <- rbind(flip_round, true_round)

# % Spanish conversions for each of five trade-off arms surveyed (6 options per arm)
high_spanish_perc <- c(8, 12, 17, 24, 35, 53)
low_spanish_perc <- c(10, 14, 18, 22, 27, 32)
equal_spanish_perc <- c(8, 13, 18, 23, 28, 33)
flip_low_spanish_perc <- c(8, 15, 21, 27, 32, 35)
flip_high_spanish_perc <- c(5, 21, 35, 44, 51, 57)
num_opt <- 6
comparison_points <- data.frame(matrix(NA, nrow=5*num_opt)) 
comparison_points$slope_factor <- c(rep("High",num_opt),rep("Low",num_opt),rep("Equal",num_opt),
                                    rep("Low_Flip",num_opt),rep("High_Flip",num_opt))
comparison_points$int_spanish_perc <- c(high_spanish_perc, low_spanish_perc, equal_spanish_perc,
                                        flip_low_spanish_perc, flip_high_spanish_perc)

comparison_options <- as.data.frame(comparison_points) %>%
  select(slope_factor, int_spanish_perc) %>%
  inner_join(round, by=c('slope_factor', 'int_spanish_perc')) %>%
  inner_join(five_arm_basedf, by = c('slope_factor', 'slope', 'newslopenames', 'int_spanish_perc', 'int_spanish_convs', 'int_english_convs',
                                         'int_total_convs', 'spanish_conversions', 'english_conversions', 'total_conversions',
                                         'spanish_perc'))

##################################################################################################
############################ PLOT PARETO CURVES (Figs 3 and S7) ##################################
##################################################################################################

# Fig 3 Pareto Frontier (high trade-off survey arm)
high_pareto_with_points <- ggplot() + 
  geom_line(five_arm_basedf %>% filter(slope=='High'), mapping = aes(x=spanish_perc, y=total_conversions,
                                                                     color=slope_factor)) +
  geom_point(comparison_options %>% filter(slope=='High'), mapping = aes(x=spanish_perc, y=total_conversions,
                                                                         color=slope_factor),
             size=3) +
  geom_vline(xintercept=0.23, linetype='dashed') +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(size=15),
        axis.title.x=element_text(size=15,margin=margin(10,0,0,0)),
        axis.title.y=element_text(size=15,margin=margin(0,10,0,0)),
        plot.margin=margin(10,25,10,10),
        axis.ticks=element_blank()) +
  scale_x_continuous(labels = scales::percent) +
  labs(x="% Spanish Conversions Allocated",
       y="Total Conversions (English and Spanish)",
       color="Trade-off Slope",
       shape="Surveyed Population")

high_pareto_with_points
ggsave(paste0(plot_dir,"Fig3.png"), high_pareto_with_points,
       width=8, height=6, dpi=300)

# Fig 7 Pareto Frontier (all survey arms)
pareto_with_points <- ggplot() + 
  geom_line(five_arm_basedf, mapping = aes(x=spanish_perc, y=total_conversions,
                                               color=slope_factor)) +
  geom_point(comparison_options, mapping = aes(x=spanish_perc, y=total_conversions,
                                               color=slope_factor),
             size=3) +
  geom_vline(xintercept=0.23, linetype='dashed') +
  scale_shape_manual(values=c(3,1),
                     guide = guide_legend(reverse=TRUE)) +
  theme_bw() +
  theme(legend.position = c(0.90,0.75),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        axis.text=element_text(size=15),
        axis.title.x=element_text(size=15,margin=margin(10,0,0,0)),
        axis.title.y=element_text(size=15,margin=margin(0,10,0,0)),
        plot.margin=margin(10,25,10,10),
        axis.ticks=element_blank()) +
  scale_x_continuous(labels = scales::percent) +
  #scale_y_continuous(expand = c(0, 0)) +
  labs(x="% Spanish Conversions Allocated",
       y="Total Conversions (English and Spanish)",
       color="Trade-off Slope",
       shape="Surveyed Population")

pareto_with_points
ggsave(paste0(plot_dir,"SuppFig7.png"), pareto_with_points,
       width=8, height=6, dpi=300)

##################################################################################################
############################ GENERATE PICTOGRAMS (Figure 2) ######################################
##################################################################################################

MakePictograms <- function(df, prefix, nrow=4){
  
  option_set <- df %>% filter(slope_factor == prefix)
  max_tot <- max(option_set$int_total_convs)
  ncols <- floor(max_tot/nrow)
  
  # save pictograms as png
  wp <- paste0('wp',seq(nrow(option_set)))
  for(i in (1:nrow(option_set))){
    spanish_count <- option_set$int_spanish_convs[i]
    english_count <- option_set$int_english_convs[i]
    tot_count <- option_set$int_total_convs[i]
    phantom_count <- max_tot - tot_count
    png(paste0(pict_dir,prefix,'_waffle',i,'.png'), res = 72, width=480, height=200)
    wp <- waffle(c(Spanish=spanish_count, English=english_count, phantom_count), 
                 rows = nrow, 
                 size = 0,
                 use_glyph = "male", 
                 glyph_size = 10, #nrow,#-1.25,
                 equal = FALSE,
                 legend_pos = "left",
                 colors = c("#1879bf", "#969696", "white")) +  #"#c7d4b6", "#a3aabd"
      theme(legend.text = element_text(size = 14))
    plot(wp)
    dev.off()
  }
}

MakePictograms(comparison_options, 'High')
MakePictograms(comparison_options, 'Low')
MakePictograms(comparison_options, 'Equal')

MakeFlipPictograms <- function(df, prefix, nrow=4){
  
  option_set <- df %>% filter(slope_factor == prefix)
  max_tot <- max(option_set$int_total_convs) #max(df$int_total_convs)
  ncols <- floor(max_tot/nrow)
  
  # save pictograms as png
  wp <- paste0('wp',seq(nrow(option_set)))
  for(i in (1:nrow(option_set))){
    spanish_count <- option_set$int_spanish_convs[i]
    english_count <- option_set$int_english_convs[i]
    tot_count <- option_set$int_total_convs[i]
    phantom_count <- max_tot - tot_count
    png(paste0(pict_dir,prefix,'_waffle',i,'.png'), res = 72, width=480, height=200)
    wp <- waffle(c(English=english_count, Spanish=spanish_count, phantom_count), 
                 rows = nrow, 
                 size = 0,
                 use_glyph = "male", 
                 glyph_size = 10,
                 equal = FALSE,
                 legend_pos = "left",
                 colors = c("#1879bf", "#969696", "white")) +  #"#c7d4b6", "#a3aabd"
      theme(legend.text = element_text(size = 14))
    plot(wp)
    dev.off()
  }
}

MakeFlipPictograms(comparison_options, 'Low_Flip')
MakeFlipPictograms(comparison_options, 'High_Flip')
