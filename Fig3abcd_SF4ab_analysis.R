require("tidyverse")
require("ggridges")
#
#load data 
#set the path in the lines below to wherever you store the data locally
#
fixed_masses_glu_allreps <- read.csv("Fig3ac_SF4a_mopsEZglu_withmetadata.csv", header = T)
#
fixed_masses_suc_allreps <- read.csv("Fig3bd_SF4b_mopsEZsuc_withmetadata.csv", header = T) 
#
#
#split datasets by replicate for plotting
#glucose
fixed_masses_glu_rep1 <- subset(fixed_masses_glu_allreps, fixed_masses_glu_allreps$Rep == 1)
fixed_masses_glu_rep2 <- subset(fixed_masses_glu_allreps, fixed_masses_glu_allreps$Rep == 2)
fixed_masses_glu_rep3 <- subset(fixed_masses_glu_allreps, fixed_masses_glu_allreps$Rep == 3)
fixed_masses_glu_rep4 <- subset(fixed_masses_glu_allreps, fixed_masses_glu_allreps$Rep == 4)
#succinate
fixed_masses_suc_rep1 <- subset(fixed_masses_suc_allreps, fixed_masses_suc_allreps$Rep == 1)
fixed_masses_suc_rep2 <- subset(fixed_masses_suc_allreps, fixed_masses_suc_allreps$Rep == 2)
fixed_masses_suc_rep3 <- subset(fixed_masses_suc_allreps, fixed_masses_suc_allreps$Rep == 3)
fixed_masses_suc_rep4 <- subset(fixed_masses_suc_allreps, fixed_masses_suc_allreps$Rep == 4)
#
#
rep1_glu_groupmedian <- tapply(fixed_masses_glu_rep1$Mass, fixed_masses_glu_rep1$CumulativeTime_hrs, median)
rep1_glu_groupIQR <- tapply(fixed_masses_glu_rep1$Mass, fixed_masses_glu_rep1$CumulativeTime_hrs, IQR, na.rm = T)
rep1_glu_groupmean <- tapply(fixed_masses_glu_rep1$Mass, fixed_masses_glu_rep1$CumulativeTime_hrs, mean)
rep1_glu_groupsd <- tapply(fixed_masses_glu_rep1$Mass, fixed_masses_glu_rep1$CumulativeTime_hrs, sd)
rep1_glu_groupcv <- rep1_glu_groupsd/rep1_glu_groupmean
rep1_glu_group_Time_hrs <- c(unique(fixed_masses_glu_rep1$CumulativeTime_hrs))
#make a new dataframe of statistics
rep1_glu_stats <- data.frame(rep1_glu_group_Time_hrs, rep1_glu_groupmedian, rep1_glu_groupmean, rep1_glu_groupIQR, rep1_glu_groupsd)
rep1_glu_stats$rep1_glu_group_rCV <- 0.741*rep1_glu_stats$rep1_glu_groupIQR / rep1_glu_stats$rep1_glu_groupmedian
rep1_glu_stats$rep1_glu_group_CV <- rep1_glu_stats$rep1_glu_groupsd / rep1_glu_stats$rep1_glu_groupmean
rep1_glu_stats$log_rep1_glu_groupmedian <- log(rep1_glu_stats$rep1_glu_groupmedian)
rep1_glu_stats$log_rep1_glu_groupmean <- log(rep1_glu_stats$rep1_glu_groupmean)
rep1_glu_stats$rep <- rep(1, 7)
names(rep1_glu_stats) <- c("Time_hrs", "medianmass", "meanmass", "IQR", "stddev", "rCV", "CV", "log_medianmass", "log_meanmass", "rep")
rep1_glu_stats
#
rep2_glu_groupmedian <- tapply(fixed_masses_glu_rep2$Mass, fixed_masses_glu_rep2$CumulativeTime_hrs, median)
rep2_glu_groupIQR <- tapply(fixed_masses_glu_rep2$Mass, fixed_masses_glu_rep2$CumulativeTime_hrs, IQR, na.rm = T)
rep2_glu_groupmean <- tapply(fixed_masses_glu_rep2$Mass, fixed_masses_glu_rep2$CumulativeTime_hrs, mean)
rep2_glu_groupsd <- tapply(fixed_masses_glu_rep2$Mass, fixed_masses_glu_rep2$CumulativeTime_hrs, sd)
rep2_glu_groupcv <- rep2_glu_groupsd/rep2_glu_groupmean
rep2_glu_group_Time_hrs <- c(unique(fixed_masses_glu_rep2$CumulativeTime_hrs))
#make a new dataframe of statistics
rep2_glu_stats <- data.frame(rep2_glu_group_Time_hrs, rep2_glu_groupmedian, rep2_glu_groupmean, rep2_glu_groupIQR, rep2_glu_groupsd)
rep2_glu_stats$rep2_glu_group_rCV <- 0.741*rep2_glu_stats$rep2_glu_groupIQR / rep2_glu_stats$rep2_glu_groupmedian
rep2_glu_stats$rep2_glu_group_CV <- rep2_glu_stats$rep2_glu_groupsd / rep2_glu_stats$rep2_glu_groupmean
rep2_glu_stats$log_rep2_glu_groupmedian <- log(rep2_glu_stats$rep2_glu_groupmedian)
rep2_glu_stats$log_rep2_glu_groupmean <- log(rep2_glu_stats$rep2_glu_groupmean)
rep2_glu_stats$rep <- rep(2, 7)
names(rep2_glu_stats) <- c("Time_hrs", "medianmass", "meanmass", "IQR", "stddev", "rCV", "CV", "log_medianmass", "log_meanmass", "rep")
rep2_glu_stats
#
rep3_glu_groupmedian <- tapply(fixed_masses_glu_rep3$Mass, fixed_masses_glu_rep3$CumulativeTime_hrs, median)
rep3_glu_groupIQR <- tapply(fixed_masses_glu_rep3$Mass, fixed_masses_glu_rep3$CumulativeTime_hrs, IQR, na.rm = T)
rep3_glu_groupmean <- tapply(fixed_masses_glu_rep3$Mass, fixed_masses_glu_rep3$CumulativeTime_hrs, mean)
rep3_glu_groupsd <- tapply(fixed_masses_glu_rep3$Mass, fixed_masses_glu_rep3$CumulativeTime_hrs, sd)
rep3_glu_groupcv <- rep3_glu_groupsd/rep3_glu_groupmean
rep3_glu_group_Time_hrs <- c(unique(fixed_masses_glu_rep3$CumulativeTime_hrs))
#make a new dataframe of statistics
rep3_glu_stats <- data.frame(rep3_glu_group_Time_hrs, rep3_glu_groupmedian, rep3_glu_groupmean, rep3_glu_groupIQR, rep3_glu_groupsd)
rep3_glu_stats$rep3_glu_group_rCV <- 0.741*rep3_glu_stats$rep3_glu_groupIQR / rep3_glu_stats$rep3_glu_groupmedian
rep3_glu_stats$rep3_glu_group_CV <- rep3_glu_stats$rep3_glu_groupsd / rep3_glu_stats$rep3_glu_groupmean
rep3_glu_stats$log_rep3_glu_groupmedian <- log(rep3_glu_stats$rep3_glu_groupmedian)
rep3_glu_stats$log_rep3_glu_groupmean <- log(rep3_glu_stats$rep3_glu_groupmean)
rep3_glu_stats$rep <- rep(3, 7)
names(rep3_glu_stats) <- c("Time_hrs", "medianmass", "meanmass", "IQR", "stddev", "rCV", "CV", "log_medianmass", "log_meanmass", "rep")
rep3_glu_stats
#
rep4_glu_groupmedian <- tapply(fixed_masses_glu_rep4$Mass, fixed_masses_glu_rep4$CumulativeTime_hrs, median)
rep4_glu_groupIQR <- tapply(fixed_masses_glu_rep4$Mass, fixed_masses_glu_rep4$CumulativeTime_hrs, IQR, na.rm = T)
rep4_glu_groupmean <- tapply(fixed_masses_glu_rep4$Mass, fixed_masses_glu_rep4$CumulativeTime_hrs, mean)
rep4_glu_groupsd <- tapply(fixed_masses_glu_rep4$Mass, fixed_masses_glu_rep4$CumulativeTime_hrs, sd)
rep4_glu_groupcv <- rep4_glu_groupsd/rep4_glu_groupmean
rep4_glu_group_Time_hrs <- c(unique(fixed_masses_glu_rep4$CumulativeTime_hrs))
#make a new dataframe of statistics
rep4_glu_stats <- data.frame(rep4_glu_group_Time_hrs, rep4_glu_groupmedian, rep4_glu_groupmean, rep4_glu_groupIQR, rep4_glu_groupsd)
rep4_glu_stats$rep4_glu_group_rCV <- 0.741*rep4_glu_stats$rep4_glu_groupIQR / rep4_glu_stats$rep4_glu_groupmedian
rep4_glu_stats$rep4_glu_group_CV <- rep4_glu_stats$rep4_glu_groupsd / rep4_glu_stats$rep4_glu_groupmean
rep4_glu_stats$log_rep4_glu_groupmedian <- log(rep4_glu_stats$rep4_glu_groupmedian)
rep4_glu_stats$log_rep4_glu_groupmean <- log(rep4_glu_stats$rep4_glu_groupmean)
rep4_glu_stats$rep <- rep(4, 5)
names(rep4_glu_stats) <- c("Time_hrs", "medianmass", "meanmass", "IQR", "stddev", "rCV", "CV", "log_medianmass", "log_meanmass", "rep")
rep4_glu_stats
#
rep1_suc_groupmedian <- tapply(fixed_masses_suc_rep1$Mass, fixed_masses_suc_rep1$CumulativeTime_hrs, median)
rep1_suc_groupIQR <- tapply(fixed_masses_suc_rep1$Mass, fixed_masses_suc_rep1$CumulativeTime_hrs, IQR, na.rm = T)
rep1_suc_groupmean <- tapply(fixed_masses_suc_rep1$Mass, fixed_masses_suc_rep1$CumulativeTime_hrs, mean)
rep1_suc_groupsd <- tapply(fixed_masses_suc_rep1$Mass, fixed_masses_suc_rep1$CumulativeTime_hrs, sd)
rep1_suc_groupcv <- rep1_suc_groupsd/rep1_suc_groupmean
rep1_suc_group_Time_hrs <- c(unique(fixed_masses_suc_rep1$CumulativeTime_hrs))
#make a new dataframe of statistics
rep1_suc_stats <- data.frame(rep1_suc_group_Time_hrs, rep1_suc_groupmedian, rep1_suc_groupmean, rep1_suc_groupIQR, rep1_suc_groupsd)
rep1_suc_stats$rep1_suc_group_rCV <- 0.741*rep1_suc_stats$rep1_suc_groupIQR / rep1_suc_stats$rep1_suc_groupmedian
rep1_suc_stats$rep1_suc_group_CV <- rep1_suc_stats$rep1_suc_groupsd / rep1_suc_stats$rep1_suc_groupmean
rep1_suc_stats$log_rep1_suc_groupmedian <- log(rep1_suc_stats$rep1_suc_groupmedian)
rep1_suc_stats$log_rep1_suc_groupmean <- log(rep1_suc_stats$rep1_suc_groupmean)
rep1_suc_stats$rep <- rep(1, 7)
names(rep1_suc_stats) <- c("Time_hrs", "medianmass", "meanmass", "IQR", "stddev", "rCV", "CV", "log_medianmass", "log_meanmass", "rep")
rep1_suc_stats
#
rep2_suc_groupmedian <- tapply(fixed_masses_suc_rep2$Mass, fixed_masses_suc_rep2$CumulativeTime_hrs, median)
rep2_suc_groupIQR <- tapply(fixed_masses_suc_rep2$Mass, fixed_masses_suc_rep2$CumulativeTime_hrs, IQR, na.rm = T)
rep2_suc_groupmean <- tapply(fixed_masses_suc_rep2$Mass, fixed_masses_suc_rep2$CumulativeTime_hrs, mean)
rep2_suc_groupsd <- tapply(fixed_masses_suc_rep2$Mass, fixed_masses_suc_rep2$CumulativeTime_hrs, sd)
rep2_suc_groupcv <- rep2_suc_groupsd/rep2_suc_groupmean
rep2_suc_group_Time_hrs <- c(unique(fixed_masses_suc_rep2$CumulativeTime_hrs))
#make a new dataframe of statistics
rep2_suc_stats <- data.frame(rep2_suc_group_Time_hrs, rep2_suc_groupmedian, rep2_suc_groupmean, rep2_suc_groupIQR, rep2_suc_groupsd)
rep2_suc_stats$rep2_suc_group_rCV <- 0.741*rep2_suc_stats$rep2_suc_groupIQR / rep2_suc_stats$rep2_suc_groupmedian
rep2_suc_stats$rep2_suc_group_CV <- rep2_suc_stats$rep2_suc_groupsd / rep2_suc_stats$rep2_suc_groupmean
rep2_suc_stats$log_rep2_suc_groupmedian <- log(rep2_suc_stats$rep2_suc_groupmedian)
rep2_suc_stats$log_rep2_suc_groupmean <- log(rep2_suc_stats$rep2_suc_groupmean)
rep2_suc_stats$rep <- rep(2, 7)
names(rep2_suc_stats) <- c("Time_hrs", "medianmass", "meanmass", "IQR", "stddev", "rCV", "CV", "log_medianmass", "log_meanmass", "rep")
rep2_suc_stats
#
rep3_suc_groupmedian <- tapply(fixed_masses_suc_rep3$Mass, fixed_masses_suc_rep3$CumulativeTime_hrs, median)
rep3_suc_groupIQR <- tapply(fixed_masses_suc_rep3$Mass, fixed_masses_suc_rep3$CumulativeTime_hrs, IQR, na.rm = T)
rep3_suc_groupmean <- tapply(fixed_masses_suc_rep3$Mass, fixed_masses_suc_rep3$CumulativeTime_hrs, mean)
rep3_suc_groupsd <- tapply(fixed_masses_suc_rep3$Mass, fixed_masses_suc_rep3$CumulativeTime_hrs, sd)
rep3_suc_groupcv <- rep3_suc_groupsd/rep3_suc_groupmean
rep3_suc_group_Time_hrs <- c(unique(fixed_masses_suc_rep3$CumulativeTime_hrs))
#make a new dataframe of statistics
rep3_suc_stats <- data.frame(rep3_suc_group_Time_hrs, rep3_suc_groupmedian, rep3_suc_groupmean, rep3_suc_groupIQR, rep3_suc_groupsd)
rep3_suc_stats$rep3_suc_group_rCV <- 0.741*rep3_suc_stats$rep3_suc_groupIQR / rep3_suc_stats$rep3_suc_groupmedian
rep3_suc_stats$rep3_suc_group_CV <- rep3_suc_stats$rep3_suc_groupsd / rep3_suc_stats$rep3_suc_groupmean
rep3_suc_stats$log_rep3_suc_groupmedian <- log(rep3_suc_stats$rep3_suc_groupmedian)
rep3_suc_stats$log_rep3_suc_groupmean <- log(rep3_suc_stats$rep3_suc_groupmean)
rep3_suc_stats$rep <- rep(3, 7)
names(rep3_suc_stats) <- c("Time_hrs", "medianmass", "meanmass", "IQR", "stddev", "rCV", "CV", "log_medianmass", "log_meanmass", "rep")
rep3_suc_stats
#
rep4_suc_groupmedian <- tapply(fixed_masses_suc_rep4$Mass, fixed_masses_suc_rep4$CumulativeTime_hrs, median)
rep4_suc_groupIQR <- tapply(fixed_masses_suc_rep4$Mass, fixed_masses_suc_rep4$CumulativeTime_hrs, IQR, na.rm = T)
rep4_suc_groupmean <- tapply(fixed_masses_suc_rep4$Mass, fixed_masses_suc_rep4$CumulativeTime_hrs, mean)
rep4_suc_groupsd <- tapply(fixed_masses_suc_rep4$Mass, fixed_masses_suc_rep4$CumulativeTime_hrs, sd)
rep4_suc_groupcv <- rep4_suc_groupsd/rep4_suc_groupmean
rep4_suc_group_Time_hrs <- c(unique(fixed_masses_suc_rep4$CumulativeTime_hrs))
#make a new dataframe of statistics
rep4_suc_stats <- data.frame(rep4_suc_group_Time_hrs, rep4_suc_groupmedian, rep4_suc_groupmean, rep4_suc_groupIQR, rep4_suc_groupsd)
rep4_suc_stats$rep4_suc_group_rCV <- 0.741*rep4_suc_stats$rep4_suc_groupIQR / rep4_suc_stats$rep4_suc_groupmedian
rep4_suc_stats$rep4_suc_group_CV <- rep4_suc_stats$rep4_suc_groupsd / rep4_suc_stats$rep4_suc_groupmean
rep4_suc_stats$log_rep4_suc_groupmedian <- log(rep4_suc_stats$rep4_suc_groupmedian)
rep4_suc_stats$log_rep4_suc_groupmean <- log(rep4_suc_stats$rep4_suc_groupmean)
rep4_suc_stats$rep <- rep(4, 5)
names(rep4_suc_stats) <- c("Time_hrs", "medianmass", "meanmass", "IQR", "stddev", "rCV", "CV", "log_medianmass", "log_meanmass", "rep")
rep4_suc_stats
#
#
#combine stats data for all reps of glucose
stats_glu <- rbind(rep1_glu_stats, rep2_glu_stats, rep3_glu_stats, rep4_glu_stats)
stats_glu
#
#combine stats data for all reps of succinate
stats_suc <- rbind(rep1_suc_stats, rep2_suc_stats, rep3_suc_stats, rep4_suc_stats)
stats_suc
#
#
#RIDGEPLOT GLUCOSE
#
temp_3A_rep1 <- ggplot(fixed_masses_glu_rep1, aes(x = Mass, y = CumulativeTime_hrs, group = CumulativeTime_hrs, alpha = 0.4)) + ggridges::geom_density_ridges(scale = 3, show.legend = FALSE, fill = "gray80", linetype = "dotted") +  scale_x_continuous(name = "Buoyant Mass (fg)", limits = c(10, 2500), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000), labels = c(expression("1x10"^1), "", "", "", expression("5x10"^1), "", "", "", "", expression("1x10"^2), "", "", "", expression("5x10"^2), "", "", "", "", expression("1x10"^3), ""), trans = "log10", sec.axis = sec_axis(trans = ~.*1, breaks = c(50, 500), labels = c(0.001, 0.01), expression("OD"[600]))) + scale_y_continuous(name = "Time (hours)", limits = c(0,9), breaks = seq(0, 8, 1), labels = c(0,"", 2,"", 4,"", 6, "", 8)) + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), axis.ticks.y.right = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "white"), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(colour = "white"), axis.title.x = element_text(size = 32))
temp_3A_rep1
#
#invert axes of previous plot
#glu_rep1_bw_ridge_inverted <- glu_rep1_bw_ridge +coord_flip()
#glu_rep1_bw_ridge_inverted
temp_3A_allreps <- temp_3A_rep1 + ggridges::geom_density_ridges(data = fixed_masses_glu_rep2, aes(x = Mass, y = CumulativeTime_hrs, group = CumulativeTime_hrs, alpha = 0.4, linetype = "dashed"), scale = 3, show.legend = FALSE, fill = "gray60") + ggridges::geom_density_ridges(data = fixed_masses_glu_rep3, aes(x = Mass, y = CumulativeTime_hrs, group = CumulativeTime_hrs, alpha = 0.4, linetype = "longdash"), scale = 3, show.legend = FALSE, fill = "gray40") + ggridges::geom_density_ridges(data = fixed_masses_glu_rep4, aes(x = Mass, y = CumulativeTime_hrs, group = CumulativeTime_hrs, alpha = 0.4, linetype = "solid"), scale = 3, show.legend = FALSE, fill = "gray20")
#
#invert axes of previous plot
Figure3A <- temp_3A_allreps +coord_flip()
Figure3A
#
#
#RIDGEPLOT SUCCINATE
#
temp_3B_rep1 <- ggplot(fixed_masses_suc_rep1, aes(x = Mass, y = CumulativeTime_hrs, group = CumulativeTime_hrs, alpha = 0.4)) + ggridges::geom_density_ridges(scale = 3, show.legend = FALSE, fill = "gray80", linetype = "dotted") +  scale_x_continuous(name = "Buoyant Mass (fg)", limits = c(10, 2500), breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000), labels = c(expression("1x10"^1), "", "", "", expression("5x10"^1), "", "", "", "", expression("1x10"^2), "", "", "", expression("5x10"^2), "", "", "", "", expression("1x10"^3), ""), trans = "log10", sec.axis = sec_axis(trans = ~.*1, breaks = c(50, 500), labels = c(0.001, 0.01), expression("OD"[600]))) + scale_y_continuous(name = "Time (hours)", limits = c(0,9), breaks = seq(0, 8, 1), labels = c(0,"", 2,"", 4,"", 6, "", 8)) + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), axis.ticks.y.right = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "white"), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(colour = "white"), axis.title.x = element_text(size = 32))
temp_3B_rep1
#
temp_3B_allreps <- temp_3B_rep1 + ggridges::geom_density_ridges(data = fixed_masses_suc_rep2, aes(x = Mass, y = CumulativeTime_hrs, group = CumulativeTime_hrs, alpha = 0.4, linetype = "dashed"), scale = 3, show.legend = FALSE, fill = "gray60") + ggridges::geom_density_ridges(data = fixed_masses_suc_rep3, aes(x = Mass, y = CumulativeTime_hrs, group = CumulativeTime_hrs, alpha = 0.4, linetype = "longdash"), scale = 3, show.legend = FALSE, fill = "gray40") + ggridges::geom_density_ridges(data = fixed_masses_suc_rep4, aes(x = Mass, y = CumulativeTime_hrs, group = CumulativeTime_hrs, alpha = 0.4, linetype = "solid"), scale = 3, show.legend = FALSE, fill = "gray20")
#
#invert axes of previous plot
Figure3B <- temp_3B_allreps +coord_flip()
Figure3B
#
#
#Figure 3C
#Median mass 
#Glucose
Figure3C <- ggplot(data = stats_glu, aes(y= medianmass, x = Time_hrs)) +geom_rect(aes(xmin = 135/60, xmax = 269/60, ymin = 50, ymax = 800), colour = "gray80", fill = "gray80") +geom_point(shape = 21, fill = "black", size = 5, alpha = 0.8) +geom_line(aes(group = as.factor(rep)), linetype = "solid") +scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, 1), labels = c(0, "", 2, "", 4, "", 6, "", 8)) +scale_y_continuous(limits = c(50, 800), breaks = c(50, 60, 70, 80, 90, 100, 200, 300, 400, 500), labels = c(expression("5x10"^1), "", "", "", "", "", "", "", "", expression("5x10"^2)), trans = "log", sec.axis = sec_axis(trans = ~.*1, breaks = c(50, 500), labels = c(0.001, 0.01), expression("OD"[600]))) + xlab('Time (hours)') + ylab('Median cell buoyant mass (fg)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"), axis.ticks.y.right = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "white"), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(colour = "white"), axis.title.x = element_text(size = 32))
Figure3C
#
#
#Figure 3D
#Median mass 
#Succinate
Figure3D <- ggplot(data = stats_suc, aes(y= medianmass, x = Time_hrs)) +geom_rect(aes(xmin = 135/60, xmax = 269/60, ymin = 50, ymax = 800), colour = "gray80", fill = "gray80") +geom_point(shape = 21, fill = "black", size = 5, alpha = 0.8) +geom_line(aes(group = as.factor(rep)), linetype = "solid") +scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, 1), labels = c(0, "", 2, "", 4, "", 6, "", 8)) +scale_y_continuous(limits = c(50, 800), breaks = c(50, 60, 70, 80, 90, 100, 200, 300, 400, 500), labels = c(expression("5x10"^1), "", "", "", "", "", "", "", "", expression("5x10"^2)), trans = "log", sec.axis = sec_axis(trans = ~.*1, breaks = c(50, 500), labels = c(0.001, 0.01), expression("OD"[600]))) + xlab('Time (hours)') + ylab('Median cell buoyant mass (fg)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"), axis.ticks.y.right = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "white"), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(colour = "white"), axis.title.x = element_text(size = 32))
Figure3D
#
#
#Supplemental Figure 4A
#plot of CV and rCV vs time of each replicate
#Glucose
SuppFigure4A <- ggplot(data = stats_glu, aes(y= rCV, x = Time_hrs)) +geom_rect(aes(xmin = 135/60, xmax = 269/60, ymin = 0, ymax = 1.5), colour = "gray80", fill = "gray80") +geom_point(shape = 21, fill = "black", size = 5, alpha = 0.8) +geom_line(aes(group = as.factor(rep)), linetype = "solid")  +scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 1), labels = c(0, "", 2, "", 4, "", 6, "", 8)) +scale_y_continuous(limits = c(0, 1.5), breaks = c(0, 0.5, 1, 1.5), labels = c(0, 0.5, 1, 1.5)) + xlab('Time (hours)') + ylab('robust CV cell buoyant mass') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32))
SuppFigure4A
#
#Supplemental Figure 4B
#plot of CV and rCV vs time of each replicate
#Succinate
SuppFigure4B <- ggplot(data = stats_suc, aes(y= rCV, x = Time_hrs)) +geom_rect(aes(xmin = 135/60, xmax = 269/60, ymin = 0, ymax = 1.5), colour = "gray80", fill = "gray80") +geom_point(shape = 21, fill = "black", size = 5, alpha = 0.8) +geom_line(aes(group = as.factor(rep)), linetype = "solid")  +scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 1), labels = c(0, "", 2, "", 4, "", 6, "", 8)) +scale_y_continuous(limits = c(0, 1.5), breaks = c(0, 0.5, 1, 1.5), labels = c(0, 0.5, 1, 1.5)) + xlab('Time (hours)') + ylab('robust CV cell buoyant mass') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks.x = element_line(colour = "black"), axis.ticks.y = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.x = element_text(size = 32))
SuppFigure4B