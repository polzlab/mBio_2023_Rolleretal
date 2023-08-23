require(ggplot2)
#
#load data
fixed_OD_conc_glu <- read.csv("Fig3ef_mopsEZglu_withmetadata.csv", header = T)
#succinate ez
fixed_OD_conc_suc <- read.csv("Fig3ef_mopsEZsuc_withmetadata.csv", header = T)
#
#
#estimate growth rate during the steady-state period using either cell conc or OD data
#
#MOPS+EZ+ACGU+Glucose 
#trim data to include only within steady-state
glu_steadystate <- subset(fixed_OD_conc_glu, fixed_OD_conc_glu$RelTime >= 135)
glu_steadystate <- subset(glu_steadystate, glu_steadystate$RelTime <= 269)
#split steady-state data by rep
glu_steadystate_rep1 <- subset(glu_steadystate, glu_steadystate$Rep == 1)
glu_steadystate_rep2 <- subset(glu_steadystate, glu_steadystate$Rep == 2)
glu_steadystate_rep3 <- subset(glu_steadystate, glu_steadystate$Rep == 3)
glu_steadystate_rep4 <- subset(glu_steadystate, glu_steadystate$Rep == 4)
#
#cell conc growth rate estimate
#fit growth rates using linear regression of log transformed cell conc vs. time in hours
lm_rep1_glu_conc <- lm(log(conc_dilcorr) ~ CumulativeTime_hrs, data = glu_steadystate_rep1)
lm_rep2_glu_conc <- lm(log(conc_dilcorr) ~ CumulativeTime_hrs, data = glu_steadystate_rep2)
lm_rep3_glu_conc <- lm(log(conc_dilcorr) ~ CumulativeTime_hrs, data = glu_steadystate_rep3)
lm_rep4_glu_conc <- lm(log(conc_dilcorr) ~ CumulativeTime_hrs, data = glu_steadystate_rep4)
#extract growth rate (slope) from linear models for each replicate and save as a vector
growthrate_glu_cellconc <- c(coef(lm_rep1_glu_conc)[2], coef(lm_rep2_glu_conc)[2], coef(lm_rep3_glu_conc)[2], coef(lm_rep4_glu_conc)[2])
#calculate mean and sd of growth rate of 4 reps
mean(growthrate_glu_cellconc)
sd(growthrate_glu_cellconc)
#
#OD growth rate estimate
#fit growth rates using linear regression of log transformed od vs. time in hours
lm_rep1_glu_od <- lm(logOD600_bkgcorr_dilcorr ~ CumulativeTime_hrs, data = glu_steadystate_rep1)
lm_rep2_glu_od <- lm(logOD600_bkgcorr_dilcorr ~ CumulativeTime_hrs, data = glu_steadystate_rep2)
lm_rep3_glu_od <- lm(logOD600_bkgcorr_dilcorr ~ CumulativeTime_hrs, data = glu_steadystate_rep3)
lm_rep4_glu_od <- lm(logOD600_bkgcorr_dilcorr ~ CumulativeTime_hrs, data = glu_steadystate_rep4)
#extract growth rate (slope) from linear models for each replicate and save as a vector
growthrate_glu_od <- c(coef(lm_rep1_glu_od)[2], coef(lm_rep2_glu_od)[2], coef(lm_rep3_glu_od)[2], coef(lm_rep4_glu_od)[2])
#calculate mean and sd of growth rate of 4 reps
mean(growthrate_glu_od)
sd(growthrate_glu_od)
#
#
#MOPS+EZ+ACGU+Succinate 
#trim data to include only within steady-state
suc_steadystate <- subset(fixed_OD_conc_suc, fixed_OD_conc_suc$RelTime >= 135)
suc_steadystate <- subset(suc_steadystate, suc_steadystate$RelTime <= 269)
#split steady-state data by rep
suc_steadystate_rep1 <- subset(suc_steadystate, suc_steadystate$Rep == 1)
suc_steadystate_rep2 <- subset(suc_steadystate, suc_steadystate$Rep == 2)
suc_steadystate_rep3 <- subset(suc_steadystate, suc_steadystate$Rep == 3)
suc_steadystate_rep4 <- subset(suc_steadystate, suc_steadystate$Rep == 4)
#
#cell conc growth rate estimate
#fit growth rates using linear regression of log transformed cell conc vs. time in hours
lm_rep1_suc_conc <- lm(log(conc_dilcorr) ~ CumulativeTime_hrs, data = suc_steadystate_rep1)
lm_rep2_suc_conc <- lm(log(conc_dilcorr) ~ CumulativeTime_hrs, data = suc_steadystate_rep2)
lm_rep3_suc_conc <- lm(log(conc_dilcorr) ~ CumulativeTime_hrs, data = suc_steadystate_rep3)
lm_rep4_suc_conc <- lm(log(conc_dilcorr) ~ CumulativeTime_hrs, data = suc_steadystate_rep4)
#extract growth rate (slope) from linear models for each replicate and save as a vector
growthrate_suc_cellconc <- c(coef(lm_rep1_suc_conc)[2], coef(lm_rep2_suc_conc)[2], coef(lm_rep3_suc_conc)[2], coef(lm_rep4_suc_conc)[2])
#calculate mean and sd of growth rate of 4 reps
mean(growthrate_suc_cellconc)
sd(growthrate_suc_cellconc)
#
#OD growth rate estimate
#fit growth rates using linear regression of log transformed od vs. time in hours
lm_rep1_suc_od <- lm(logOD600_bkgcorr_dilcorr ~ CumulativeTime_hrs, data = suc_steadystate_rep1)
lm_rep2_suc_od <- lm(logOD600_bkgcorr_dilcorr ~ CumulativeTime_hrs, data = suc_steadystate_rep2)
lm_rep3_suc_od <- lm(logOD600_bkgcorr_dilcorr ~ CumulativeTime_hrs, data = suc_steadystate_rep3)
lm_rep4_suc_od <- lm(logOD600_bkgcorr_dilcorr ~ CumulativeTime_hrs, data = suc_steadystate_rep4)
#extract growth rate (slope) from linear models for each replicate and save as a vector
growthrate_suc_od <- c(coef(lm_rep1_suc_od)[2], coef(lm_rep2_suc_od)[2], coef(lm_rep3_suc_od)[2], coef(lm_rep4_suc_od)[2])
mean(growthrate_suc_od)
sd(growthrate_suc_od)
#
#
#PLOTS 
#MOPS+EZ+ACGU+Glucose
#split dataset of Glucose for drawing lines to connect consecutive datapoints
fixed_OD_glu <- fixed_OD_conc_glu[27:63,]
fixed_conc_glu <- fixed_OD_conc_glu[1:26,]
#
expected_OD_glu <- data.frame(exp_y = c(1.9778*1E5, 1.7762*1E5, 1.86566*1E5, 1.8743*1E5), exp_x = c(0,0,0,0), Rep = c(1,2,3,4)) 
#
Figure3e <- ggplot(data = fixed_OD_conc_glu) +geom_rect(aes(xmin = 135/60, xmax = 269/60, ymin = 1E5, ymax = 5E9), colour = "gray80", fill = "gray80") +geom_point(aes(y = conc_dilcorr, x = CumulativeTime_hrs), fill = "black", size = 5, alpha = 0.6) +geom_point(aes(y = newvar_scaled, x = CumulativeTime_hrs), size = 5, alpha = 0.6, colour = "firebrick", fill = "firebrick") +geom_point(data = expected_OD_glu, aes(y = exp_y, x = exp_x), colour = "firebrick", fill = "white", size = 5, alpha = 0.4, shape = 21) +geom_line(data = fixed_conc_glu, aes(y = conc_dilcorr, x = CumulativeTime_hrs, group = as.factor(Rep)), linetype = "solid") +geom_line(data = fixed_OD_glu, aes(y = newvar_scaled, x = CumulativeTime_hrs, group = as.factor(Rep)), linetype = "solid", colour = "firebrick") +scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, 1), labels = c(0,"", 2,"", 4,"", 6, "", 8)) +scale_y_continuous(limits = c(1E5, 5E9), breaks = c(1E5, 2E5, 3E5, 4E5, 5E5, 6E5, 7E5, 8E5, 9E5, 1E6, 2E6, 3E6, 4E6, 5E6, 6E6, 7E6, 8E6, 9E6, 1E7, 2E7, 3E7, 4E7, 5E7, 6E7, 7E7, 8E7, 9E7, 1E8, 2E8, 3E8, 4E8, 5E8, 6E8, 7E8, 8E8, 9E8, 1E9, 2E9, 3E9, 4E9, 5E9), labels = c(expression("1x10"^5), "", "", "", "", "", "", "", "", expression("1x10"^6),"", "", "", "", "", "", "", "", expression("1x10"^7), "", "", "", "", "", "", "", "", expression("1x10"^8),"", "", "", "", "", "", "", "", expression("1x10"^9), "", "", "", ""), trans = "log", name = "Cell concentration (cells/ml)", sec.axis = sec_axis(trans = ~.*1, breaks = c(1E6, 1E7, 1E8, 1E9), labels = c(0.001, 0.01, 0.1, 1),name = expression("OD"[600]))) + xlab('Time (hours)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "firebrick", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(size = 32, angle = 90, colour = "firebrick"), axis.title.x = element_text(size = 32))
Figure3e
#
#
#MOPS+EZ+ACGU+Succinate
#split dataset of Succinate for drawing lines to connect consecutive datapoints
fixed_OD_suc <- fixed_OD_conc_suc[27:63,]
fixed_conc_suc <- fixed_OD_conc_suc[1:26,]
#
expected_OD_suc <- data.frame(exp_y = c(3.554*1E5, 3.3303*1E5,3.439*1E5, 2.9332*1E5), exp_x = c(0,0,0,0), Rep = c(1,2,3,4)) 
#
Figure3f <- ggplot(data = fixed_OD_conc_suc) +geom_rect(aes(xmin = 135/60, xmax = 269/60, ymin = 1E5, ymax = 5E9), colour = "gray80", fill = "gray80") +geom_point(aes(y = conc_dilcorr, x = CumulativeTime_hrs), fill = "black", size = 5, alpha = 0.6) +geom_point(aes(y = newvar_scaled, x = CumulativeTime_hrs), size = 5, alpha = 0.6, colour = "firebrick", fill = "firebrick") +geom_point(data = expected_OD_suc, aes(y = exp_y, x = exp_x), colour = "firebrick", fill = "white", size = 5, alpha = 0.4, shape = 21) +geom_line(data = fixed_conc_suc, aes(y = conc_dilcorr, x = CumulativeTime_hrs, group = as.factor(Rep)), linetype = "solid") +geom_line(data = fixed_OD_suc, aes(y = newvar_scaled, x = CumulativeTime_hrs, group = as.factor(Rep)), linetype = "solid", colour = "firebrick") +scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, 1), labels = c(0,"", 2,"", 4,"", 6, "", 8)) +scale_y_continuous(limits = c(1E5, 5E9), breaks = c(1E5, 2E5, 3E5, 4E5, 5E5, 6E5, 7E5, 8E5, 9E5, 1E6, 2E6, 3E6, 4E6, 5E6, 6E6, 7E6, 8E6, 9E6, 1E7, 2E7, 3E7, 4E7, 5E7, 6E7, 7E7, 8E7, 9E7, 1E8, 2E8, 3E8, 4E8, 5E8, 6E8, 7E8, 8E8, 9E8, 1E9, 2E9, 3E9, 4E9, 5E9), labels = c(expression("1x10"^5), "", "", "", "", "", "", "", "", expression("1x10"^6),"", "", "", "", "", "", "", "", expression("1x10"^7), "", "", "", "", "", "", "", "", expression("1x10"^8),"", "", "", "", "", "", "", "", expression("1x10"^9), "", "", "", ""), trans = "log", name = "Cell concentration (cells/ml)", sec.axis = sec_axis(trans = ~.*1, breaks = c(1E6, 1E7, 1E8, 1E9), labels = c(0.001, 0.01, 0.1, 1),name = expression("OD"[600]))) + xlab('Time (hours)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "firebrick", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(size = 32, angle = 90, colour = "firebrick"), axis.title.x = element_text(size = 32))
Figure3f