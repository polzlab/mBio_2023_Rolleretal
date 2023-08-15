#
require(ggplot2)
#
#Load data
data_fig2h <- read.csv("/Volumes/rollerb21/Data/Manuscripts/SteadyStateMass/_publicdata/Figure2h/Fig2h_withMetadata.csv", header = T)
data_fig2h
#
#split data into only OD and only cell conc datasets for connecting consecutive datapoints with a line
data_fig2h_onlyOD <- data_fig2h[1:27,]
data_fig2h_onlycellconc <- data_fig2h[28:51,]
#
#Plots
#
expected_OD <- data.frame(exp_y = c(4.8771*1E6, 5.208*1E6), exp_x = c(0,0)) 
#
#+geom_hline(yintercept = 2.75e+7, colour = "black", linetype = "dotted")
#+geom_hline(yintercept = 1.5e+08, colour = "firebrick", linetype = "dotted")
OD600_cellconc_27July_1E3 <- ggplot(data = data_fig2h, aes(x = Time_hrs)) +geom_point(aes(y = conc_dilcorr), fill = "black", size = 5, alpha = 0.6, shape = 21) +geom_point(aes(y = OD600_bkgcorr_dilcorr*1E9), shape = 21, size = 5, alpha = 0.6, colour = "firebrick", fill = "firebrick") +geom_point(data = expected_OD, aes(y = exp_y, x = exp_x), colour = "firebrick", fill = "white", size = 5, alpha = 0.6, shape = 21) +geom_line(data = data_fig2h_onlycellconc, aes(y = conc_dilcorr, group = as.factor(Culture)), linetype = "solid") +geom_line(data = data_fig2h_onlyOD, aes(y = OD600_bkgcorr_dilcorr*1E9, group = as.factor(Culture)), linetype = "solid", colour = "firebrick") +scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, 1), labels = c(0,"", 2,"", 4,"", 6, "", 8)) +scale_y_continuous(limits = c(1E5, 5E9), breaks = c(1E5, 2E5, 3E5, 4E5, 5E5, 6E5, 7E5, 8E5, 9E5, 1E6, 2E6, 3E6, 4E6, 5E6, 6E6, 7E6, 8E6, 9E6, 1E7, 2E7, 3E7, 4E7, 5E7, 6E7, 7E7, 8E7, 9E7, 1E8, 2E8, 3E8, 4E8, 5E8, 6E8, 7E8, 8E8, 9E8, 1E9, 2E9, 3E9, 4E9, 5E9), labels = c(expression("1x10"^5), "", "", "", "", "", "", "", "", expression("1x10"^6),"", "", "", "", "", "", "", "", expression("1x10"^7), "", "", "", "", "", "", "", "", expression("1x10"^8),"", "", "", "", "", "", "", "", expression("1x10"^9), "", "", "", ""), trans = "log", name = "Cell concentration (cells/ml)", sec.axis = sec_axis(trans = ~.*1, breaks = c(1E6, 1E7, 1E8, 1E9), labels = c(0.001, 0.01, 0.1, 1),name = expression("OD"[600]))) + xlab('Time (hours)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "firebrick", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(size = 32, angle = 90, colour = "firebrick"), axis.title.x = element_text(size = 32))
OD600_cellconc_27July_1E3
tiff("/Volumes/rollerb21/Data/Manuscripts/SteadyStateMass/_publicdata/Figure2h/Figure2h.tiff", width = 8, height = 8, unit = "in", res = 300)
OD600_cellconc_27July_1E3
dev.off()
