require("tidyverse")
require("ggridges")
#load data
#set the path in the line below to wherever you store the data locally
#
data_fig2a <- read.csv("Fig2a2d_SF3a_Ecoli_1E4_Data_withMetadata.csv")
head(data_fig2a)
tail(data_fig2a)
#
#
#split dataset by replicate
data_fig2a_rep1 <- subset(data_fig2a, data_fig2a$rep == "rep1")
data_fig2a_rep2 <- subset(data_fig2a, data_fig2a$rep == "rep2")
#
#
#calculate mean and median buoyant mass by well for each replicate
#rep1
rep1_groupmean <- tapply(data_fig2a_rep1$Mass, data_fig2a_rep1$SampleTime, mean)
rep1_groupmedian <- tapply(data_fig2a_rep1$Mass, data_fig2a_rep1$SampleTime, median)
rep1_groupq1 <- tapply(data_fig2a_rep1$Mass, data_fig2a_rep1$SampleTime, quantile , 0.25, na.rm = T)
rep1_groupq3 <- tapply(data_fig2a_rep1$Mass, data_fig2a_rep1$SampleTime, quantile , 0.75, na.rm = T)
rep1_groupIQR <- tapply(data_fig2a_rep1$Mass, data_fig2a_rep1$SampleTime, IQR)
rep1_group_Time_min <- sort(unique(data_fig2a_rep1$SampleTime))
#make a new dataframe of statistics
rep1_stats <- data.frame("Time_min" = rep1_group_Time_min, "meanmass" = rep1_groupmean, "medianmass" = rep1_groupmedian, "Q1" = rep1_groupq1, "Q3" = rep1_groupq3, "IQR" = rep1_groupIQR)
rep1_stats$rCV <- 0.741*rep1_stats$IQR / rep1_stats$medianmass
rep1_stats$Time_hrs <- rep1_stats$Time_min /60
rep1_stats$rep <- rep(1, 11)
#
rep1_stats
#
#rep2
rep2_groupmean <- tapply(data_fig2a_rep2$Mass, data_fig2a_rep2$SampleTime, mean)
rep2_groupmedian <- tapply(data_fig2a_rep2$Mass, data_fig2a_rep2$SampleTime, median)
rep2_groupq1 <- tapply(data_fig2a_rep2$Mass, data_fig2a_rep2$SampleTime, quantile , 0.25, na.rm = T)
rep2_groupq3 <- tapply(data_fig2a_rep2$Mass, data_fig2a_rep2$SampleTime, quantile , 0.75, na.rm = T)
rep2_groupIQR <- tapply(data_fig2a_rep2$Mass, data_fig2a_rep2$SampleTime, IQR)
rep2_group_Time_min <- sort(unique(data_fig2a_rep2$SampleTime))
#make a new dataframe of statistics
rep2_stats <- data.frame("Time_min" = rep2_group_Time_min, "meanmass" = rep2_groupmean, "medianmass" = rep2_groupmedian, "Q1" = rep2_groupq1, "Q3" = rep2_groupq3, "IQR" = rep2_groupIQR)
rep2_stats$rCV <- 0.741*rep2_stats$IQR / rep2_stats$medianmass
rep2_stats$Time_hrs <- rep2_stats$Time_min /60
rep2_stats$rep <- rep(2, 11)
#
rep2_stats
#
#combine summary stats
bothreps_stats <- rbind(rep1_stats, rep2_stats)
bothreps_stats
#
#
#PLOTS
#
#Figure 2a step 1
#first make plot with replicate 1
temp_2A_rep1 <- ggplot(data_fig2a_rep1, aes(x = Mass, y = SampleTime_hrs, group = SampleTime_hrs, alpha = 0.3)) + ggridges::geom_density_ridges(scale = 3, show.legend = FALSE, fill = "black", linetype = "dashed") +  scale_x_continuous(name = "Buoyant mass (fg)", limits = c(10, 2500), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000), labels = c(expression("1x10"^1), "", "", "", expression("5x10"^1), "", "", "", "", expression("1x10"^2), "", "", "", expression("5x10"^2), "", "", "", "", expression("1x10"^3), ""), trans = "log10", sec.axis = sec_axis(trans = ~.*1, breaks = c(50, 500), labels = c(0.001, 0.01), name = expression("OD"[600]))) + scale_y_continuous(name = "Time (hours)", limits = c(0,9), breaks = seq(0, 8, 1), labels = c(0,"", 2,"", 4,"", 6, "", 8)) + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.text.x.top = element_text(colour = "white"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank(),  panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "white"), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32), axis.title.x.top = element_text(colour = "white"), axis.title.y.right = element_text(colour = "white"))
temp_2A_rep1
#
#Figure 2a step 2
#make overlay of rep2 on top of previously made plot of rep 1
temp_2A_bothreps <- temp_2A_rep1 + ggridges::geom_density_ridges(data = data_fig2a_rep2, aes(x = Mass, y = SampleTime_hrs, group = SampleTime_hrs, alpha = 0.3), scale = 3, show.legend = FALSE, fill = "gray100")
#
#Figure 2a Final step
#invert axes of previously made overlay plot
Figure2A <- temp_2A_bothreps +coord_flip()
Figure2A
#
#
#Figure 2d
Figure2D <- ggplot(data = bothreps_stats, aes(y= medianmass, x = Time_hrs)) +geom_rect(aes(xmin = 163/60, xmax = 218/60, ymin = 50, ymax = 800), colour = "gray80", fill = "gray80") +geom_point(shape = 21, fill = "black", size = 5, alpha = 0.8) +geom_line(aes(group = as.factor(rep)), linetype = "solid")  +scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, 1), labels = c(0, "", 2, "", 4, "", 6, "", 8)) +scale_y_continuous(limits = c(50, 800), breaks = c(50, 60, 70, 80, 90, 100, 200, 300, 400, 500), labels = c(expression("5x10"^1), "", "", "", "", "", "", "", "", expression("5x10"^2)), trans = "log", sec.axis = sec_axis(trans = ~.*1, breaks = c(50, 500), labels = c(0.001, 0.01), expression("OD"[600]))) + xlab('Time (hours)') + ylab('Median cell buoyant mass (fg)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"), axis.ticks.y.right = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "white"), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(colour = "white"), axis.title.x = element_text(size = 32))
Figure2D
#
#
#Supplemental Figure 3A
#plot of CV and rCV vs time of each replicate
SuppFigure3A <- ggplot(data = bothreps_stats, aes(y= rCV, x = Time_hrs)) +geom_rect(aes(xmin = 163/60, xmax = 218/60, ymin = 0, ymax = 1.5), colour = "gray80", fill = "gray80") +geom_point(shape = 21, fill = "black", size = 5, alpha = 0.8) +geom_line(aes(group = as.factor(rep)), linetype = "solid")  +scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, 1), labels = c(0, "", 2, "", 4, "", 6, "", 8)) +scale_y_continuous(limits = c(0, 1.5), breaks = c(0, 0.5, 1, 1.5), labels = c(0, 0.5, 1, 1.5)) + xlab('Time (hours)') + ylab('robust CV cell buoyant mass') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32))
SuppFigure3A