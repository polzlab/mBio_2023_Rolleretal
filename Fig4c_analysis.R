require("tidyverse")
require("ggridges")
require("lmerTest")
#
#load data
#set the path in the line below to wherever you store the data locally
#longitudinal data
data_fig4c_longitudinal <- read.csv("Fig4C_fixed_summarystats_withMetadata.csv")
#cross-sectional data
data_fig4c_xsectional_stats <- read.csv("Fig4c_cross-sectional_summarystats_withMetadata.csv")
data_fig4c_xsectional_masses <- read.csv("Fig4c_cross-sectional_masses_withMetadata.csv")
#
#
#STATS
#glucose EZ ACGU
#mixed effect model on log transformed mass data for
lme1_glu_log <- lmer(logmass ~ Treatment + (1| Sample:Treatment) + (1 | Replicate), data = data_fig4c_xsectional_masses)
anova(lme1_glu_log)
summary(lme1_glu_log)
#confint(lme1_log)
exp(5.99246 + 0.02811)#non-fixed cell mass
exp(5.99246)#fixed cell mass
exp(5.99246 + 0.02811) - exp(5.99246) #interpret effect size of fixed (Live - Fixed) in fg
(exp(5.99246 + 0.02811) - exp(5.99246)) / exp(5.99246 + 0.02811) #interpret effect size of fixed (Live-Fixed)/Live) in % of live cell mass
#
#
#PLOTS
Fig4c_longitudinal <- ggplot(data = data_fig4c_longitudinal, aes(y= medianmass, x = Time_hrs)) +geom_point(aes(shape = as.factor(rep)), size = 5, alpha = 0.7) + geom_errorbar(aes(x= Time_hrs , ymin = Q1, ymax = Q3), colour = "black", width = 0.3) +scale_shape_manual(values = c(15, 16, 8, 18)) +scale_y_continuous(limits = c(30, 1000), breaks = c(30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), labels = c("", "", expression("5x10"^1), "", "", "", "", expression("1x10"^2), "", "", "", expression("5x10"^2), "", "", "", "", expression("1x10"^3)), trans = "log10") +scale_x_continuous(limits = c(-0.1, 9.5), breaks = seq(0, 6, 1), labels = c(0, "", 2, "", 4, "", 6)) +xlab("") + ylab("Median cell buoyant mass (fg)") + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32))
Fig4c_longitudinal

#
#
Fig4c <- Fig4c_longitudinal + geom_point(data = data_fig4c_xsectional_stats, aes(x = Time_hrs, y = medianmass, fill = Trt, shape = as.factor(rep)), alpha = 0.7, size = 5) + geom_errorbar(data = data_fig4c_xsectional_stats, aes(ymin = Q1, ymax = Q3)) +scale_shape_manual(values = c(15, 16, 8, 18, 24, 25, 25)) +scale_fill_manual(values = c("black", "white"))
Fig4c