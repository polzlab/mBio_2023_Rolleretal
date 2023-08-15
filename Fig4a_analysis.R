require("tidyverse")
require("ggridges")
require("lmerTest")
#load data
#set the path in the lines below to wherever you store the data locally
#longitudinal
bothreps_stats_live <- read.csv("Fig4A_nonfixed_summarystats_withMetadata.csv")
bothreps_stats_live
bothreps_stats_fixed <- read.csv("Fig4A_fixed_summarystats_withMetadata.csv")
bothreps_stats_fixed
#cross-sectional
xsectional_stats_all <- read.csv("Fig4a_cross-sectional_summarystats_withMetadata.csv")
xsectional_stats_all
xsectional_masses_all <- read.csv("Fig4a_cross-sectional_masses_withMetadata.csv")
head(xsectional_masses_all)
#
#
#STATS
#mixed effect model on log transformed mass data
lme1_log <- lmer(logmass ~ Treatment + (1| Sample:Treatment) + (1 | Replicate), data = xsectional_masses_all)
anova(lme1_log)
summary(lme1_log)
#
exp(6.17975+0.19603) #live cell mass 
exp(6.17975) #fixed cell mass
exp(6.17975+0.19603) - exp(6.17975)  #interpret effect size of fixed (Live - Fixed) in fg
(exp(6.17975+0.19603) - exp(6.17975)) / exp(6.17975+0.19603) #interpret effect size of fixed (Live-Fixed)/Live) in % of live cell mass
#
# 
#PLOTS
temp_Fig4a_longitudinal <- ggplot(data = bothreps_stats_live, aes(y=medianmass, x = Time_hrs)) + geom_errorbar(aes(x= Time_hrs , ymin = Q1, ymax = Q3), colour = "gray40", linetype = "dashed", width = 0.3) +geom_point(aes(shape = as.factor(rep)), size = 5, alpha = 1, colour = "black", fill = "white") +scale_shape_manual(values = c(21,22)) +scale_y_continuous(limits = c(30, 1000), breaks = c(30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), labels = c("", "", expression("5x10"^1), "", "", "", "", expression("1x10"^2), "", "", "", expression("5x10"^2), "", "", "", "", expression("1x10"^3)), trans = "log10") +scale_x_continuous(limits = c(-0.1, 9.5), breaks = seq(0, 6, 1), labels = c(0, "", 2, "", 4, "", 6)) +xlab("") + ylab("Median cell buoyant mass (fg)") + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32))
temp_Fig4a_longitudinal
#
Fig4a_longitudinal <- temp_Fig4a_longitudinal + geom_errorbar(data = bothreps_stats_fixed, aes(x= Time_hrs , ymin = Q1, ymax = Q3), colour = "black") +geom_point(data = bothreps_stats_fixed, aes(y=medianmass, x = Time_hrs, shape = as.factor(rep)), size = 5, colour = "black", fill = "black", alpha = 0.7) +scale_shape_manual(values = c(21, 22))
Fig4a_longitudinal
#
#
Fig4a <- Fig4a_longitudinal + geom_point(data = xsectional_stats_all, aes(x = Time_hrs, y = medianmass, fill = Treatment, shape = as.factor(rep)), alpha = 0.7, size = 5) + geom_errorbar(data = xsectional_stats_all, aes(ymin = Q1, ymax = Q3)) +scale_shape_manual(values = c(21, 22, 23, 24, 25)) +scale_fill_manual(values = c("black", "white"))
Fig4a