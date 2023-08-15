require("tidyverse")
#
#
#load data
#set the path in the line below to wherever you store the data locally
data_summary <- read.csv("SuppFig1c_summarystats_withmetadata.csv", header = T)
data_summary
#
#adjust dataset to long format for making plot in
#
data_summary_trim <- data_summary[, c(1,18:21)]
#
data_summary_longer <- pivot_longer(data_summary_trim, cols = 2:5, names_to = "fluid", values_to = "density")
data_summary_longer
#
#
lm1 <- aov(density ~ fluid, data = data_summary_longer)
summary(lm1)
TukeyHSD(lm1)
#
#plots
SuppFig1c <- ggplot(data = data_summary_longer, aes(y = density, x = fluid)) + geom_point(aes(shape = as.factor(beadsize)), size = 5,  alpha = 0.4) +scale_y_continuous(limits = c(1.0,1.03)) + xlab('\n Medium') + ylab(expression('Fluid density (g/cm'^3*')')) + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.text.y = element_text(size = 24, colour = "black"), axis.text.x = element_text(size = 24, colour = "white"), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32))
SuppFig1c