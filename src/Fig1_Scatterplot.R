usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("ggplot2")
usePackage("gridExtra")
usePackage("dplyr")
usePackage("tidyr")

data_dir <- ('~/data/')
plot_dir <- ('~/plots/')

################################################################################
############################ ACS VS. GCF COMPARE ###############################
################################################################################

raw.data <- read.csv(paste0(data_dir,'gcf_acs.csv'),stringsAsFactors = FALSE) # private GetCalFresh data not provided

county_scatter <- raw.data %>%
  dplyr::select(county_name, 
         acs_adults_below_poverty_speak_spanish, acs_adults_below_poverty_speak_asian_pacisl, 
         acs_pop_below_poverty_level,
         gcf_total_spanish_pref,
         gcf_total_households_applied,
         acs_total_pop) %>%
  mutate(acs_spanish_share = 100*acs_adults_below_poverty_speak_spanish/acs_pop_below_poverty_level,
         gcf_spanish_share = 100*gcf_total_spanish_pref/gcf_total_households_applied)

fig1 <- ggplot(county_scatter, aes(x=acs_spanish_share, y=gcf_spanish_share,
                                    size=acs_total_pop)) +
  geom_point(alpha=0.6) +
  geom_abline() +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(size=18),
        axis.title.x=element_text(size=15, margin=margin(10,0,0,0)),
        axis.title.y=element_text(size=15, margin=margin(0,10,0,0)),
        plot.margin=margin(10,20,10,10),
        axis.ticks=element_blank()) +
  guides(size=FALSE) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,50)) +
  scale_y_continuous(expand = c(0, 0.25), limits=c(0,50)) +
  scale_size_continuous(range=c(1,20)) +
  labs(x="% Spanish speakers living under the poverty line (by county)",
       y="% Spanish speakers among GetCalFresh applicants",
       size="Population")

fig1
ggsave(paste0(plot_dir,"Fig1.png"), fig1, height=8, width=8.5, dpi=300)

