#
require(ggplot2)
#
#Load data
#set the path in the line below to wherever you store the data locally
data_fig1g <- read.csv("Fig1g_withMetadata.csv", header = T)
data_fig1g
#
#split data into secondary datasets (only OD and only cell conc) for use in geom_line to connect consecutive datapoints with a line
data_fig1g_onlyOD <- data_fig1g[1:20,]
data_fig1g_onlycellconc <- data_fig1g[21:42,]
#
#
#Plots
#
#line 
expected_OD <- data.frame(exp_y = c(2.2046*1E5, 2.2830*1E5), exp_x = c(0,0)) 
#
Fig1g <- ggplot(data = data_fig1g, aes(x = Time_hrs))  +geom_rect(aes(xmin = 189/60, xmax = 279/60, ymin = 1E5, ymax = 5E9), colour = "gray80", fill = "gray80") +geom_point(aes(y = conc_dilcorr), fill = "black", size = 5, alpha = 0.6, shape = 21) +geom_point(aes(y = OD600_bkgcorr_dilcorr*1E9), shape = 21, size = 5, alpha = 0.6, colour = "firebrick", fill = "firebrick") +geom_point(data = expected_OD, aes(y = exp_y, x = exp_x), colour = "firebrick", fill = "white", size = 5, alpha = 0.6, shape = 21) +geom_line(data = data_fig1g_onlycellconc, aes(y = conc_dilcorr, group = as.factor(Culture)), linetype = "solid") +geom_line(data = data_fig1g_onlyOD, aes(y = OD600_bkgcorr_dilcorr*1E9, group = as.factor(Culture)), linetype = "solid", colour = "firebrick") +scale_x_continuous(limits = c(0, 9), breaks = seq(0, 8, 1), labels = c(0,"", 2,"", 4,"", 6, "", 8)) +scale_y_continuous(limits = c(1E5, 5E9), breaks = c(1E5, 2E5, 3E5, 4E5, 5E5, 6E5, 7E5, 8E5, 9E5, 1E6, 2E6, 3E6, 4E6, 5E6, 6E6, 7E6, 8E6, 9E6, 1E7, 2E7, 3E7, 4E7, 5E7, 6E7, 7E7, 8E7, 9E7, 1E8, 2E8, 3E8, 4E8, 5E8, 6E8, 7E8, 8E8, 9E8, 1E9, 2E9, 3E9, 4E9, 5E9), labels = c(expression("1x10"^5), "", "", "", "", "", "", "", "", expression("1x10"^6),"", "", "", "", "", "", "", "", expression("1x10"^7), "", "", "", "", "", "", "", "", expression("1x10"^8),"", "", "", "", "", "", "", "", expression("1x10"^9), "", "", "", ""), trans = "log", name = "Cell concentration (cells/ml)", sec.axis = sec_axis(trans = ~.*1, breaks = c(1E6, 1E7, 1E8, 1E9), labels = c(0.001, 0.01, 0.1, 1),name = expression("OD"[600]))) + xlab('Time (hours)') + theme(legend.position = "none", strip.background = element_blank(), strip.text = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), axis.ticks = element_line(colour = "black"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_text(colour = "black", size = 24), axis.text.y.right = element_text(colour = "firebrick", size = 24), axis.text.x = element_text(colour = "black", size = 24), axis.title.y = element_text(size = 32, angle = 90), axis.title.y.right = element_text(size = 32, angle = 90, colour = "firebrick"), axis.title.x = element_text(size = 32))
Fig1g