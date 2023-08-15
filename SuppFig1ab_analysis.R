#load packages
require("ggplot2")
#
#load data
#set the path in the line below to wherever you store the data locally
data_4andless <- read.csv("SuppFig1ab_calibrationdata.csv", header = T)
#
#filter out data above 3um beads (642.19987 fg expected mass)
data_3andless <- subset(data_4andless, data_4andless$exp_mass < 643)
data_3andless
#
#
#STATS
#
#stats for Supp Fig 1A
lm1 <- lm(median_mass ~ exp_mass, data = data_3andless)
summary(lm1)
confint(lm1)
#
#stats for Supp Fig1B
lm2 <- lm(median_mass ~ exp_mass, data = data_4andless)
summary(lm2)
confint(lm2)
#
#
#PLOTS
#
SuppFig1A <- ggplot(data = data_3andless, aes(y = median_mass, x = exp_mass)) + geom_point(size = 3, shape = 21, alpha = 0.4) +geom_smooth(method = 'glm', se = T, linewidth = 0.1, colour = "black") +scale_y_continuous(limits = c(0,1000)) +scale_x_continuous(limits = c(0,1000)) + xlab('Expected buoyant mass (fg)') + ylab('Median buoyant mass (fg)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), panel.grid.major = element_line(colour = "gray80"), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.text.y = element_text(size = 24, colour = "black"), axis.text.x = element_text(size = 24, colour = "black"), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32))
SuppFig1A
#
#
SuppFig1B <- ggplot(data = data_4andless, aes(y = median_mass, x = exp_mass)) + geom_point(size = 3, shape = 21, alpha = 0.4) +geom_smooth(method = 'glm', se = T, linewidth = 0.1, colour = "black") +scale_y_continuous(limits = c(0,2000)) +scale_x_continuous(limits = c(0,2000)) + xlab('Expected buoyant mass (fg)') + ylab('Median buoyant mass (fg)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), panel.grid.major = element_line(colour = "gray80"), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.text.y = element_text(size = 24, colour = "black"), axis.text.x = element_text(size = 24, colour = "black"), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32))
SuppFig1B

