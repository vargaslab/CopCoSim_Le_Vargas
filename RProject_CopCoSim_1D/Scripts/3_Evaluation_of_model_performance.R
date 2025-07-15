###                 (3) Evaluation of model performance                      ###

# Create a folder to store results for Final Figures 
dir.create(paste(getwd(),"/Results/Time_Series/Final_Figures", sep=""))
FF_dir<-paste(root_dir,"/Results/Time_Series/Final_Figures",sep="")

#--------------------------------Figure 2--------------------------------------#
# Initialize variables and settings for Figure 2
log_list=list(Temp,CO2)      # List of variables to be plotted
n_logs= length(log_list)     # Number of variables
log_xlabel_list=list(Templabel,CO2label) # Corresponding x-axis labels
plot_mean=TRUE               # Option to plot mean
plot_median=TRUE             # Option to plot median
fontproportion=1.0           # Font size scaling

# Save Figure 2 as a PNG image
png(paste(FF_dir,"/Figure2.png",sep=""), 
    bg = "white", width = 2000, height = 1000, res = 150 )
# Set up plotting parameters
par(mfrow = c(2,2),mar = c(4, 6, 2, 2), cex.lab= 1.6, cex.axis = 1.6)
# Plot 1: Temp vs. Time with gridlines
plot(Time, Temp,  type = "p", lwd = 2, pch = 1, 
     ylab = Templabel , xlab = Timelabel, bty="o")
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
# Add additional overlaid plot (optional, duplicated plot structure)
par(new=TRUE)
plot(Time, Temp, type = "p", lwd = 2, pch = 1, 
     ylab =Templabel , xlab = Timelabel, bty="o")
# Plot 2: CO2 vs. Time with gridlines
plot(Time, CO2,  type = "p", lwd = 2, pch = 1, 
     ylab = CO2label , xlab = Timelabel, bty="o")
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
# Overlay additional CO2 plot
par(new=TRUE)
plot(Time, CO2, type = "p", lwd = 2, pch = 1, 
     ylab =CO2label , xlab = Timelabel, bty="o")
# Plot 3: Temp vs. Time with additional sample overlay
plot(Time, Temp,  type = "p", lwd = 2, pch = 1,
     ylab = Templabel , xlab = Timelabel, bty="o", col = "lightgray")
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
# Overlay aclhs_sub48 samples
par(new=TRUE)
plot(Time_aclhs_sub48,Temp_aclhs_sub48, pch = 18,  cex = 1.5,  bty="o", col = "darkblue", xlim = c(min(Time), max(Time)), ylim=c(min(Temp), max(Temp)), xlab =Timelabel, ylab = Templabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
# Repeat the overlay for clarity
par(new=TRUE)
plot(Time_aclhs_sub48,Temp_aclhs_sub48, pch = 18,  cex = 1.5, bty="o", col = "darkblue", xlim = c(min(Time), max(Time)), ylim=c(min(Temp), max(Temp)), xlab =Timelabel, ylab = Templabel)

# Plot 4: CO2 vs. Time with aclhs_sub48 overlay
plot(Time, CO2,  type = "p", lwd = 2, pch = 1,
     ylab = CO2label , xlab = Timelabel, bty="o", col = "lightgray")
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
# Overlay aclhs_sub48 samples for CO2
par(new=TRUE)
plot(Time_aclhs_sub48,CO2_aclhs_sub48, pch = 18,  cex = 1.5,  bty="o", col = "darkblue", xlim = c(min(Time), max(Time)), ylim=c(min(CO2), max(CO2)), xlab =Timelabel, ylab = CO2label)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Time_aclhs_sub48,CO2_aclhs_sub48, pch = 18,  cex = 1.5, bty="o", col = "darkblue", xlim = c(min(Time), max(Time)), ylim=c(min(CO2), max(CO2)), xlab =Timelabel, ylab = CO2label)
box()
# Close the plotting device
dev.off()


#--------------------------------Figure 3--------------------------------------#
png(paste(FF_dir,"/Figure3.png",sep=""), 
    bg = "white", width = 2000, height = 1000, res = 150 )
par(mfrow = c(2,2),mar = c(4, 6, 2, 2), cex.lab= 1.6, cex.axis = 1.6)
plot(Time, CO2,  type = "p", lwd = 2, pch = 1, ylim=c(0, max(CO2)),
     ylab = CO2label , xlab = Timelabel, bty="o", col = "lightgray")
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Time, CO2, pch = 1,  cex = 1.5,  bty="o", col = "lightgray", xlim = c(min(Time), max(Time)), ylim=c(0, max(CO2)), xlab =Timelabel, ylab = CO2label)
par(new=TRUE)
plot(Time,sim_backTrans_CO2_nsim[,1], pch = 15,  cex = 1.2, bty="o", col = "darkred", xlim = c(min(Time), max(Time)), ylim=c(0, max(CO2)), xlab =Timelabel, ylab = CO2label)
plot(Time, CO2,  type = "p", lwd = 2, pch = 1, ylim=c(0, max(CO2)),
     ylab = CO2label , xlab = Timelabel, bty="o", col = "lightgray")
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Time, CO2, pch = 1,  cex = 1.5,  bty="o", col = "lightgray", xlim = c(min(Time), max(Time)), ylim=c(0, max(CO2)), xlab =Timelabel, ylab = CO2label)
par(new=TRUE)
plot(Time,CO2_aclhs_pred_df[1,], pch = 18,  cex = 1.5, bty="o", col = "darkgreen", xlim = c(min(Time), max(Time)), ylim=c(0, max(CO2)), xlab =Timelabel, ylab = CO2label)
plot(Time, CO2,  type = "p", lwd = 2, pch = 1, ylim=c(0, max(CO2)),
     ylab = CO2label , xlab = Timelabel, bty="o", col = "lightgray")
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Time, CO2, pch = 1,  cex = 1.5,  bty="o", col = "lightgray", xlim = c(min(Time), max(Time)), ylim=c(0, max(CO2)), xlab =Timelabel, ylab = CO2label)
for (i in 1:10) { 
  par(new=TRUE)
  plot(Time,sim_backTrans_CO2_nsim[,i], pch = 15,  cex = 1.2, bty="o", col = "darkred", xlim = c(min(Time), max(Time)), ylim=c(0, max(CO2)), xlab =Timelabel, ylab = CO2label)
}
plot(Time, CO2,  type = "p", lwd = 2, pch = 1, ylim=c(0, max(CO2)),
     ylab = CO2label , xlab = Timelabel, bty="o", col = "lightgray")
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Time, CO2, pch = 1,  cex = 1.5,  bty="o", col = "lightgray", xlim = c(min(Time), max(Time)), ylim=c(0, max(CO2)), xlab =Timelabel, ylab = CO2label)
for (i in 1:10) { 
  par(new=TRUE)
  plot(Time,CO2_aclhs_pred_df[i,], pch = 18,  cex = 1.5, bty="o", col = "darkgreen", xlim = c(min(Time), max(Time)), ylim=c(0, max(CO2)), xlab =Timelabel, ylab = CO2label)
}
box()
dev.off()

#----------------------------------------------------------------------------------#
#-------------------------------Validation with testing data-----------------------#
#----------------------------------------------------------------------------------#
CO2_testing <- CO2[-aclhs_sub48]
U_limit <- seq(0,1,by = 0.2)
png(paste(FF_dir,"/FigureS1_.png",sep=""),   bg = "white", width = 1200, height = 1000, res = 150)
par(mfrow = c(2,2),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
#-------------------Figure a----------------#
plot(ecdf(CO2_testing), pch = 1, yaxt = "n",  main = " ", xlab= CO2label, col = "lightgray",
     ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),xlim =c(min(CO2_testing),max(CO2_testing)), ylim = c(0,1))
axis(2, at=U_limit)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(ecdf(CO2_testing), pch = 1, yaxt = "n",  main = " ", xlab= CO2label, col = "lightgray",
     ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),xlim =c(min(CO2_testing),max(CO2_testing)), ylim = c(0,1))
axis(2, at=U_limit)
par(new=TRUE)
plot(ecdf(sim_backTrans_CO2_nsim[-aclhs_sub48,1]),pch = 15, col = "darkred", cex = 1.2,
     yaxt = "n",  main = " ", xlab= CO2label, ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),
     xlim =c(min(CO2),max(CO2)), ylim = c(0,1))
box()
#-------------------Figure b----------------#
plot(ecdf(CO2_testing), pch = 1, yaxt = "n",  main = " ", xlab= CO2label, col = "lightgray",
     ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),xlim =c(min(CO2_testing),max(CO2_testing)), ylim = c(0,1))
axis(2, at=U_limit)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(ecdf(CO2_testing), pch = 1, yaxt = "n",  main = " ", xlab= CO2label, col = "lightgray",
     ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),xlim =c(min(CO2_testing),max(CO2_testing)), ylim = c(0,1))
par(new=TRUE)
plot(ecdf(CO2_aclhs_pred[-aclhs_sub48]),pch = 18, col = "darkgreen", cex = 1.5,
     yaxt = "n",  main = " ", xlab= CO2label, ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),
     xlim =c(min(CO2),max(CO2)), ylim = c(0,1))
axis(2, at=U_limit)
box()
#-------------------Figure c----------------#
plot(ecdf(CO2_testing), pch = 1, yaxt = "n",  main = " ", xlab= CO2label, col = "lightgray",
     ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),xlim =c(min(CO2_testing),max(CO2_testing)), ylim = c(0,1))
axis(2, at=U_limit)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(ecdf(CO2_testing), pch = 1, yaxt = "n",  main = " ", xlab= CO2label, col = "lightgray",
     ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),xlim =c(min(CO2_testing),max(CO2_testing)), ylim = c(0,1))
axis(2, at=U_limit)
for (i in 1:10) {
  par(new=TRUE)
  plot(ecdf(sim_backTrans_CO2_nsim[-aclhs_sub48,i]),pch = 15, col = "darkred", cex = 1.2,
       yaxt = "n",  main = " ", xlab= CO2label, ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),
       xlim =c(min(CO2),max(CO2)), ylim = c(0,1))
}
box()
#-------------------Figure d----------------#
plot(ecdf(CO2_testing), pch = 1, yaxt = "n",  main = " ", xlab= CO2label, col = "lightgray",
     ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),xlim =c(min(CO2_testing),max(CO2_testing)), ylim = c(0,1))
axis(2, at=U_limit)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(ecdf(CO2_testing), pch = 1, yaxt = "n",  main = " ", xlab= CO2label, col = "lightgray",
     ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),xlim =c(min(CO2_testing),max(CO2_testing)), ylim = c(0,1))
axis(2, at=U_limit)
for (i in 1:10) {
  par(new=TRUE)
  plot(ecdf(CO2_aclhs_pred_df[i,-aclhs_sub48]),pch = 18, col = "darkgreen", cex = 1.5,
       yaxt = "n",  main = " ", xlab= CO2label, ylab = expression(bold(paste(Fn(CO[2] ~ efflux)))),
       xlim =c(min(CO2),max(CO2)), ylim = c(0,1))
}
box()
dev.off()

#------------------------------- Figure S2--------------------------------------#
Variolabel <- expression(bold("Semivariance"))
# Estimation of the experimental variogram
N_lags<- 8 #length(t)/2.5
lag_value <- 1 # min(dist(t)) # or delta t
Time_testing <- Time[-aclhs_sub48]
Temp_testing <- Temp[-aclhs_sub48]
ti_testing = numeric(length(Time_testing))
CO2_testing_VarioEstimation<-Variograma(Time_testing, ti_testing, 
                                        CO2_testing, 0, 90, N_lags, lag_value, 1, "", Timelabel) 


CO2_SGCoSim_sim_VarioEstimation_df <- NULL
for (i in 1:10) {
  CO2_SGCoSim_sim_VarioEstimation<-Variograma(Time_testing, ti_testing, 
                                              sim_backTrans_CO2_nsim[-aclhs_sub48,i], 0, 90, N_lags, lag_value, 1, "", Timelabel)
  CO2_SGCoSim_sim_VarioEstimation_df <- cbind(CO2_SGCoSim_sim_VarioEstimation_df, CO2_SGCoSim_sim_VarioEstimation$Semivarianzas)
}

CO2_aclhs_sim_VarioEstimation_df <- NULL
for (i in 1:10) {
  CO2_aclhs_sim_VarioEstimation<-Variograma(Time_testing, ti_testing, 
                                            as.numeric(CO2_aclhs_pred_df[i,-aclhs_sub48]), 0, 90, N_lags, lag_value, 1, "", Timelabel) # CO2 temporal variogram
  CO2_aclhs_sim_VarioEstimation_df <- cbind(CO2_aclhs_sim_VarioEstimation_df,CO2_aclhs_sim_VarioEstimation$Semivarianzas)
}

png(paste(FF_dir,"/FigureS2_.png",sep=""), bg = "white",  width = 1200, height = 1000, res = 150)
par(mfrow = c(2,2),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
# (a)
plot(CO2_testing_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(CO2_testing_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel ,ylab = Variolabel )
par(new=TRUE)
plot(CO2_SGCoSim_sim_VarioEstimation_df[,1], pch = 15, cex = 1.2, col = "darkred",  xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel , ylab = Variolabel)
lines.variomodel(cov.model = "sph", cov.pars = c(CO2_aclhs_sub48_sill_and_nugget-CO2_aclhs_sub48_nugget, CO2_aclhs_sub48_range)
                 , nug = CO2_aclhs_sub48_nugget, col = "Black", lwd = 3, max.dist = 1*8)
box()

# (b)
plot(CO2_testing_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(CO2_testing_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel ,ylab = Variolabel )
par(new=TRUE)
plot(CO2_aclhs_sim_VarioEstimation_df[,1], pch = 18, cex = 1.5, col = "darkgreen", xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel , ylab = Variolabel)
par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(CO2_aclhs_sub48_sill_and_nugget-CO2_aclhs_sub48_nugget, CO2_aclhs_sub48_range)
                 , nug = CO2_aclhs_sub48_nugget, col = "Black", lwd = 3, max.dist = 1*8)
box()

# (c)
plot(CO2_testing_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(CO2_testing_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel ,ylab = Variolabel )
for (i in 1:10) {
  par(new=TRUE)
  plot(CO2_SGCoSim_sim_VarioEstimation_df[,i], pch = 15, cex = 1.2, col = "darkred",  xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
       xlab = Timelabel , ylab = Variolabel)
}

lines.variomodel(cov.model = "sph", cov.pars = c(CO2_aclhs_sub48_sill_and_nugget-CO2_aclhs_sub48_nugget, CO2_aclhs_sub48_range)
                 , nug = CO2_aclhs_sub48_nugget, col = "Black", lwd = 3, max.dist = 1*8)
box()

# (d)
plot(CO2_testing_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(CO2_testing_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
     xlab = Timelabel ,ylab = Variolabel )
for (i in 1:10) {
  par(new=TRUE)
  plot(CO2_aclhs_sim_VarioEstimation_df[,i], pch = 18, cex = 1.5, col = "darkgreen", xlim = c(0,9), ylim=c(0,max(CO2_testing_VarioEstimation[,3]*3)),
       xlab = Timelabel , ylab = Variolabel)
}
par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(CO2_aclhs_sub48_sill_and_nugget-CO2_aclhs_sub48_nugget, CO2_aclhs_sub48_range)
                 , nug = CO2_aclhs_sub48_nugget, col = "Black", lwd = 3, max.dist = 1*8)
box()

dev.off()

#---------------------------------------------FigureS3--------------------------------#
Fn_Temp_testing <- estandarizar(cbind(Temp_testing,CO2_testing))[,1]
Fn_CO2_testing <- estandarizar(cbind(Temp_testing,CO2_testing))[,2]
U_limit <- seq(0,1,by = 0.2)
png(paste(FF_dir,"/FigureS3_.png",sep=""),   bg = "white", width = 1200, height = 1000, res = 150)
par(mfrow = c(2,2),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
#-------------------Figure a----------------#
plot(Temp_testing, CO2_testing, pch = 1, xlab= Templabel, ylab = CO2label,  
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(CO2_testing), max(CO2_testing)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_testing, CO2_testing, pch = 1, xlab= Templabel, ylab = CO2label, 
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(CO2_testing), max(CO2_testing)))
par(new=TRUE)
plot(Temp_testing, sim_backTrans_CO2_nsim[-aclhs_sub48,1], pch = 15, xlab= Templabel, ylab = CO2label, 
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "darkred",
     ylim = c(min(CO2_testing), max(CO2_testing)))
box()

#----------------- Figure b----------------#
plot(Temp_testing, CO2_testing, pch = 1, xlab= Templabel, ylab = CO2label,  
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(CO2_testing), max(CO2_testing)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_testing, CO2_testing, pch = 1, xlab= Templabel, ylab = CO2label, 
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(CO2_testing), max(CO2_testing)))
par(new=TRUE)
plot(Temp_testing, CO2_aclhs_pred[-aclhs_sub48],  xlab= Templabel, ylab = CO2label,
     pch = 18, col = "darkgreen", cex = 1.5,
     xlim = c(min(Temp_testing), max(Temp_testing)),  ylim = c(min(CO2_testing), max(CO2_testing)))
box()

#----------------- Figure d----------------#
plot(Temp_testing, CO2_testing, pch = 1, xlab= Templabel, ylab = CO2label,  
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(CO2_testing), max(CO2_testing)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_testing, CO2_testing, pch = 1, xlab= Templabel, ylab = CO2label, 
     xlim = c(min(Temp_testing), max(Temp_testing)),  col = "lightgray",
     ylim = c(min(CO2_testing), max(CO2_testing)))
for (i in 1:10) {
  par(new=TRUE)
  plot(Temp_testing, sim_backTrans_CO2_nsim[-aclhs_sub48,i],  xlab= Templabel, ylab = CO2label,
       pch = 15, col = "darkred", cex = 1.2,
       xlim = c(min(Temp_testing), max(Temp_testing)),  ylim = c(min(CO2_testing), max(CO2_testing)))
}
box()

#----------------- Figure d----------------#
plot(Temp_testing, CO2_testing, pch = 1, xlab= Templabel, ylab = CO2label,  
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(CO2_testing), max(CO2_testing)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_testing, CO2_testing, pch = 1, xlab= Templabel, ylab = CO2label, 
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(CO2_testing), max(CO2_testing)))
for (i in 1:10) {
  par(new=TRUE)
  plot(Temp_testing, CO2_aclhs_pred_df[i,-aclhs_sub48],  xlab= Templabel, ylab = CO2label,
       pch = 18, col = "darkgreen", cex = 1.2,
       xlim = c(min(Temp_testing), max(Temp_testing)),  ylim = c(min(CO2_testing), max(CO2_testing)))
}
box()

dev.off()

#------------------- tables-------------------#
# Statistical Properties
# Calculate statistical properties for SGCoSim and CopCoSim predictions
CO2_testing_Stat <- Estadisticas(CO2_testing)
CO2_testing_SGCoSim_pred_Stat <- Estadisticas(sim_backTrans_CO2_nsim[-aclhs_sub48,1])
CO2_testing_aclhs_pred_Stat <- Estadisticas(CO2_aclhs_pred[-aclhs_sub48])
cbind(CO2_testing_Stat,CO2_testing_SGCoSim_pred_Stat,CO2_testing_aclhs_pred_Stat)

# Kolmogorov-Smirnov (KS) Tests
# Perform KS tests to compare the distributions of predictions against the sample
ks.test(sim_backTrans_CO2_nsim[-aclhs_sub48,1], CO2_fixed_sub48, conf.level = 0.95)
ks.test(as.numeric(CO2_aclhs_pred[-aclhs_sub48]), CO2_aclhs_sub48, conf.level = 0.95)

# Perform KS tests comparing predictions and CO2 data
ks_test_SGCoSim_CO2_testing <- ks.test(sim_backTrans_CO2_nsim[-aclhs_sub48,1], CO2_testing, conf.level = 0.95)
ks_test_aclhs_CO2_testing <- ks.test(as.numeric(CO2_aclhs_pred[-aclhs_sub48]), CO2_testing, conf.level = 0.95)
ks_test_SGCoSim_CO2_testing   # Display the result for SGCoSim
ks_test_aclhs_CO2_testing     # Display the result for CopCoSim

# Calculate total absolute errors between predictions and observed CO2
sum(abs(sim_backTrans_CO2_nsim[-aclhs_sub48,1]-CO2_testing))  # Absolute error for SGCoSim
sum(abs(as.numeric(CO2_aclhs_pred[-aclhs_sub48])-CO2_testing))   # Absolute error for CopCoSim

# Loop through multiple simulations for KS tests
for (i in 1:10) {
  print(ks.test(sim_backTrans_CO2_nsim[-aclhs_sub48,i], CO2_testing, conf.level = 0.95))
}

for (i in 1:10) {
  print(ks.test(CO2_aclhs_pred_df[i,-aclhs_sub48], CO2_testing, conf.level = 0.95))
}

# Calculate and display correlations between Temp and CO2 using different methods
# Data
(corP_Temp_CO2_testing <- round(cor(Temp_testing,CO2_testing, method = "pearson"),3))
(corS_Temp_CO2_testing <- round(cor(Temp_testing,CO2_testing, method = "spearman"),3))
(corK_Temp_CO2_testing <- round(cor(Temp_testing,CO2_testing, method = "kendall"),3))
# Correlations for SGCoSim predictions
(corP_Temp_CO2_testing_SGCoSim_pred <- round(cor(Temp_testing,sim_backTrans_CO2_nsim[-aclhs_sub48,1], method = "pearson"),3))
(corS_Temp_CO2_testing_SGCoSim_pred <- round(cor(Temp_testing,sim_backTrans_CO2_nsim[-aclhs_sub48,1], method = "spearman"),3))
(corK_Temp_CO2_testing_SGCoSim_pred <- round(cor(Temp_testing,sim_backTrans_CO2_nsim[-aclhs_sub48,1], method = "kendall"),3))
# Correlations for CopCoSim predictions
(corP_Temp_CO2_testing_aclhs_pred <- round(cor(Temp_testing,CO2_aclhs_pred[-aclhs_sub48], method = "pearson"),3))
(corS_Temp_CO2_testing_aclhs_pred <- round(cor(Temp_testing,CO2_aclhs_pred[-aclhs_sub48], method = "spearman"),3))
(corK_Temp_CO2_testing_aclhs_pred <- round(cor(Temp_testing,CO2_aclhs_pred[-aclhs_sub48], method = "kendall"),3))

# Absolute differences between original and predicted correlations
abs(corP_Temp_CO2_testing_SGCoSim_pred-corP_Temp_CO2_testing)
abs(corS_Temp_CO2_testing_SGCoSim_pred-corS_Temp_CO2_testing)
abs(corK_Temp_CO2_testing_SGCoSim_pred-corK_Temp_CO2_testing)

abs(corP_Temp_CO2_testing_aclhs_pred-corP_Temp_CO2_testing)
abs(corS_Temp_CO2_testing_aclhs_pred-corS_Temp_CO2_testing)
abs(corK_Temp_CO2_testing_aclhs_pred-corK_Temp_CO2_testing)

# Relative Errors
# Calculate relative error (%) for SGCoSim and CopCoSim predictions
(sum(abs(CO2_testing-as.numeric(sim_backTrans_CO2_nsim[-aclhs_sub48,1])))/sum(CO2_testing))*100
(sum(abs(CO2_testing-as.numeric(CO2_aclhs_pred[-aclhs_sub48])))/sum(CO2_testing))*100

# Absolute Errors
# Total absolute errors for SGCoSim and CopCoSim predictions
sum(abs(CO2_testing-as.numeric(sim_backTrans_CO2_nsim[-aclhs_sub48,1])))
sum(abs(CO2_testing-as.numeric(CO2_aclhs_pred[-aclhs_sub48])))

# Error Ratio
# Ratio of SGCoSim absolute error to aclhs absolute error
sum(abs(CO2_testing-as.numeric(sim_backTrans_CO2_nsim[-aclhs_sub48,1])))/ sum(abs(CO2_testing-as.numeric(CO2_aclhs_pred[-aclhs_sub48])))

# UQ comparison
# SGCoSim
UQ_min <- apply(sim_backTrans_CO2_nsim, 1, min)
UQ_max <- apply(sim_backTrans_CO2_nsim, 1, max)
UQ_max_min <- UQ_max[-aclhs_sub48]-UQ_min[-aclhs_sub48]
UQ_max_min1 <- abs(UQ_max[-aclhs_sub48]-CO2_testing)+abs(CO2_testing-UQ_min[-aclhs_sub48])
sum(UQ_max_min)
sum(UQ_max_min1)
# CopCoSim
CopCoSim_UQ_min <- apply(CO2_aclhs_pred_df, 2, min)
CopCoSim_UQ_max <- apply(CO2_aclhs_pred_df, 2, max)
CopCoSim_UQ_max_min <- CopCoSim_UQ_max[-aclhs_sub48]-CopCoSim_UQ_min[-aclhs_sub48]
CopCoSim_UQ_max_min1 <- abs(CopCoSim_UQ_max[-aclhs_sub48]-CO2_testing)+abs(CO2_testing-CopCoSim_UQ_min[-aclhs_sub48])
sum(CopCoSim_UQ_max_min)
sum(CopCoSim_UQ_max_min1)

# Calculate total absolute errors in variogram estimation
sum(abs(CO2_SGCoSim_sim_VarioEstimation_df[,1]-CO2_testing_VarioEstimation$Semivarianzas))
sum(abs(CO2_aclhs_sim_VarioEstimation$Semivarianzas-CO2_testing_VarioEstimation$Semivarianzas))

# Ratio of variogram errors between SGCoSim and CopCoSim
sum(abs(CO2_SGCoSim_sim_VarioEstimation_df[,1]-CO2_testing_VarioEstimation$Semivarianzas))/sum(abs(CO2_aclhs_sim_VarioEstimation$Semivarianzas-CO2_testing_VarioEstimation$Semivarianzas))
