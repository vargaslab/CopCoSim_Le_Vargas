###                 (3) Evaluation of model performance                      ###

# Create a folder to store results for Final Figures 
dir.create(paste(getwd(),"/Results/spatial_2D/Final_Figures", sep=""))
FF_dir<-paste(root_dir,"/Results/spatial_2D/Final_Figures",sep="")

# Spatial distribution of the sample
par(mfrow = c(1,1),mar = c(4, 6, 2, 2), cex.lab= 1.0, cex.axis = 1.0)
plot(X,Y, xlab = Xlabel, ylab = Ylabel, xlim = c(min(X),max(X)), ylim = c(min(Y),max(Y)), pch = 0)
par(new=TRUE)
plot(X_aclhs_sub50,Y_aclhs_sub50, pch = 18, col= "blue", xlab = Xlabel, ylab = Ylabel, xlim = c(min(X),max(X)), ylim = c(min(Y),max(Y)))
box()

# Save Figure 4 as a PNG image
png(paste(FF_dir,"/Figure4a.png",sep=""), 
    bg = "white", width = 3000, height = 2000, res = 250)
DEspacial(X, Y, Temp, 
          Xlabel , Ylabel, Templabel, breaks = histf_Temp$breaks,  cex.lab = 2, cex.axis = 2)
dev.off()

png(paste(FF_dir,"/Figure4b.png",sep=""), 
    bg = "white", width = 3000, height = 2000, res = 250)
DEspacial(X, Y, Rs, 
          Xlabel , Ylabel, Rslabel, breaks = histf_Rs$breaks,  cex.lab = 2, cex.axis = 2)
dev.off()

png(paste(FF_dir,"/Figure5a.png",sep=""), 
    bg = "white", width = 3000, height = 2000, res = 250)
DEspacial(X_aclhs_sub50, Y_aclhs_sub50, Temp_aclhs_sub50, 
          Xlabel , Ylabel, Templabel, breaks = histf_Temp$breaks, cex.lab = 2, cex.axis = 2)
dev.off()

png(paste(FF_dir,"/Figure5b.png",sep=""), 
    bg = "white", width = 3000, height = 2000, res = 250)
DEspacial(X_aclhs_sub50, Y_aclhs_sub50, Rs_aclhs_sub50, 
          Xlabel , Ylabel, Rslabel, breaks = histf_Rs$breaks, cex.lab = 2, cex.axis = 2)
dev.off()

png(paste(FF_dir,"/Figure6a.png",sep=""), 
    bg = "white", width = 3000, height = 2000, res = 250)
DEspacial(X, Y, sim_backTrans_Rs_nsim[,1], 
          Xlabel , Ylabel, Rslabel, breaks = histf_Rs$breaks, cex.lab = 2, cex.axis = 2) # 
dev.off()

png(paste(FF_dir,"/Figure6b.png",sep=""), 
    bg = "white", width = 3000, height = 2000, res = 250)
DEspacial(X, Y, Rs_aclhs_nsim[,1],  # Rs_aclhs_pred
          Xlabel , Ylabel, Rslabel, breaks = histf_Rs$breaks, cex.lab = 2, cex.axis = 2) # 
dev.off()

UQ_max_min_Stat <- Estadisticas(UQ_max_min)
Rs_aclhs_nsim_UQ_Stat <- Estadisticas(Rs_aclhs_nsim_UQ)
cbind(UQ_max_min_Stat,Rs_aclhs_nsim_UQ_Stat)
histf_UQ_max_min <- HistBoxplot(x=UQ_max_min, mean = Rs_Stat[5,2], median = Rs_Stat[4,2], main ="", 
                                xlab = UQlabel, ylab = expression(bold("Relative frequency [%]")), AbsFreq = FALSE, PercentFreq = TRUE,
                                nbin =n_bins)
UQlabel <- expression(bold(paste(UQ ~ "["~ g ~ C ~ m^-2 ~"]"))) 

histf_UQ_max_min$breaks[1]  <- min(Rs_aclhs_nsim_UQ)
png(paste(FF_dir,"/Figure7a.png",sep=""), 
    bg = "white", width = 3000, height = 2000, res = 250)
DEspacial(X, Y, UQ_max_min, 
          Xlabel , Ylabel, UQlabel, breaks = c(histf_UQ_max_min$breaks, max(Rs_aclhs_nsim_UQ)),   cex.lab = 2, cex.axis = 2) # breaks = histf_Rs$breaks,
dev.off()

png(paste(FF_dir,"/Figure7b.png",sep=""), 
    bg = "white", width = 3000, height = 2000, res = 250)
DEspacial(X, Y, Rs_aclhs_nsim_UQ, 
          Xlabel , Ylabel, UQlabel, breaks = c(histf_UQ_max_min$breaks, max(Rs_aclhs_nsim_UQ)),  cex.lab = 2, cex.axis = 2) # breaks = histf_Rs$breaks,
dev.off()

#----------------------------------------------------------------------------------#
#-------------------------------Validation with testing data-----------------------#
#----------------------------------------------------------------------------------#
# Testing data
X_testing <- X[-aclhs_sub50]
Y_testing <- Y[-aclhs_sub50]
Rs_testing <- Rs[-aclhs_sub50]
Temp_testing <- Temp[-aclhs_sub50]

UQ_min <- apply(sim_backTrans_Rs_nsim[-aclhs_sub50,], 1, min)
UQ_max <- apply(sim_backTrans_Rs_nsim[-aclhs_sub50,], 1, max)
UQ_max_min <- abs(UQ_max-Rs_testing) + abs(Rs_testing-UQ_min)
sum(UQ_max_min)

Rs_aclhs_nsim[aclhs_sub50,] #<- Rs_aclhs_sub50
Rs_aclhs_nsim_UQ_min <- apply(Rs_aclhs_nsim[-aclhs_sub50,], 1, min)
Rs_aclhs_nsim_UQ_max <- apply(Rs_aclhs_nsim[-aclhs_sub50,], 1, max)
Rs_aclhs_nsim_UQ <- abs(Rs_aclhs_nsim_UQ_max-Rs_testing) + abs(Rs_testing-Rs_aclhs_nsim_UQ_min)
sum(Rs_aclhs_nsim_UQ)

sum(UQ_max_min)
sum(Rs_aclhs_nsim_UQ)
sum(UQ_max_min)/sum(Rs_aclhs_nsim_UQ)

ks.test(as.numeric(sim_backTrans_Rs_nsim[-aclhs_sub50,1]), Rs_testing, conf.level = 0.95)
sum(abs(Rs_testing-sim_backTrans_Rs_nsim[-aclhs_sub50,1]))

#---------------------------------------------FigureS4_Univariate pdfs--------------------------------#
#Fn_Temp <- estandarizar(cbind(Temp,Rs))[,1]
#Fn_Rs <- estandarizar(cbind(Temp,Rs))[,2]
U_limit <- seq(0,1,by = 0.2)
png(paste(FF_dir,"/FigureS4_.png",sep=""),   bg = "white", width = 1200, height = 1000, res = 150)
par(mfrow = c(2,2),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
#-------------------Figure a----------------#
plot(ecdf(Rs_testing), pch = 0, yaxt = "n",  main = " ", xlab= Rslabel, col = "lightgray",
     ylab = expression(bold(paste(Fn(Rs)))),xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
axis(2, at=U_limit)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(ecdf(Rs_testing), pch = 0, yaxt = "n",  main = " ", xlab= Rslabel, col = "lightgray",
     ylab = expression(bold(paste(Fn(Rs)))),xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
par(new=TRUE)
plot(ecdf(sim_backTrans_Rs_nsim[-aclhs_sub50,1]),pch = 15, col = "darkred", cex = 1.2,
     yaxt = "n",  main = " ", xlab= Rslabel, ylab = expression(bold(paste(Fn(Rs)))),
     xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
axis(2, at=U_limit)
box()

#-------------------Figure b----------------#
plot(ecdf(Rs_testing), pch = 0, yaxt = "n",  main = " ", xlab= Rslabel, col = "lightgray",
     ylab = expression(bold(paste(Fn(Rs)))),xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
axis(2, at=U_limit)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(ecdf(Rs_testing), pch = 0, yaxt = "n",  main = " ", xlab= Rslabel, col = "lightgray",
     ylab = expression(bold(paste(Fn(Rs)))),xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
axis(2, at=U_limit)
par(new=TRUE)
plot(ecdf(Rs_aclhs_nsim[-aclhs_sub50,1]),pch = 18, col = "darkgreen", cex = 1.2,
     yaxt = "n",  main = " ", xlab= Rslabel, ylab = expression(bold(paste(Fn(Rs)))),
     xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
box()

#-------------------Figure c----------------#
plot(ecdf(Rs_testing), pch = 0, yaxt = "n",  main = " ", xlab= Rslabel, col = "lightgray",
     ylab = expression(bold(paste(Fn(Rs)))),xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
axis(2, at=U_limit)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(ecdf(Rs_testing), pch = 0, yaxt = "n",  main = " ", xlab= Rslabel, col = "lightgray",
     ylab = expression(bold(paste(Fn(Rs)))),xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
axis(2, at=U_limit)
for (i in 1:10) {
  par(new=TRUE)
  plot(ecdf(sim_backTrans_Rs_nsim[-aclhs_sub50,i]),pch = 15, col = "darkred", cex = 1.2,
       yaxt = "n",  main = " ", xlab= Rslabel, ylab = expression(bold(paste(Fn(Rs)))),
       xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
}
box()

#-------------------Figure d----------------#
plot(ecdf(Rs_testing), pch = 0, yaxt = "n",  main = " ", xlab= Rslabel, col = "lightgray",
     ylab = expression(bold(paste(Fn(Rs)))),xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
axis(2, at=U_limit)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(ecdf(Rs_testing), pch = 0, yaxt = "n",  main = " ", xlab= Rslabel, col = "lightgray",
     ylab = expression(bold(paste(Fn(Rs)))),xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
axis(2, at=U_limit)
for (i in 1:10) {
  par(new=TRUE)
  plot(ecdf(Rs_aclhs_nsim[-aclhs_sub50,i]),pch = 18, col = "darkgreen", cex = 1.5,
       yaxt = "n",  main = " ", xlab= Rslabel, ylab = expression(bold(paste(Fn(Rs)))),
       xlim =c(min(Rs_testing),max(Rs_testing)), ylim = c(0,1))
}
box()
dev.off()

#------------------------------- Figure 16--------------------------------------#
Variolabel <- expression(bold("Semivariance"))
# Estimation of the experimental variogram
N_lags<- 35 #length(t)/2.5
lag_value <- 100 # min(dist(t)) # or delta t
Rs_testing_VarioEstimation<-Variograma(X_testing, Y_testing, 
                                       Rs_testing, 0, 90, N_lags, lag_value, 1, "Variograma adireccional de Rs_testing", "Distancia [m]")


Rs_SGCoSim_sim_VarioEstimation_df <- NULL
for (i in 1:10) {
  Rs_SGCoSim_sim_VarioEstimation<-Variograma(X_testing, Y_testing, 
                                             sim_backTrans_Rs_nsim[-aclhs_sub50,i], 0, 90, N_lags, lag_value, 1, "", Dislabel) # Rs Temporal variogram
  Rs_SGCoSim_sim_VarioEstimation_df <- cbind(Rs_SGCoSim_sim_VarioEstimation_df,Rs_SGCoSim_sim_VarioEstimation$Semivarianzas)
}

#png(paste(VA_dir,"/FigS6.png",sep=""), bg = "white", width = 1500, height = 1000, res = 200)
Rs_SGCoSim_sim_VarioEstimation<-Variograma(X_testing, Y_testing, 
                                           sim_backTrans_Rs_nsim[-aclhs_sub50,1], 0, 90, N_lags, lag_value, 1, "", Dislabel) # Rs Temporal variogram
#dev.off()


Rs_aclhs_sim_VarioEstimation_df <- NULL
for (i in 1:10) {
  Rs_aclhs_sim_VarioEstimation<-Variograma(X_testing, Y_testing,  
                                           Rs_aclhs_nsim[-aclhs_sub50,i], 0, 90, N_lags, lag_value, 1, "", Dislabel) # Rs Temporal variogram
  Rs_aclhs_sim_VarioEstimation_df <- cbind(Rs_aclhs_sim_VarioEstimation_df,Rs_aclhs_sim_VarioEstimation$Semivarianzas)
}

#png(paste(VA_dir,"/Rs_aclhs_sim_VarioEstimation.png",sep=""), bg = "white", width = 1500, height = 1000, res = 200)
Rs_aclhs_sim_VarioEstimation<-Variograma(X_testing, Y_testing, 
                                         Rs_aclhs_pred[-aclhs_sub50], 0, 90, N_lags, lag_value, 1, "", Dislabel) # Rs Temporal variogram
#dev.off()


png(paste(FF_dir,"/FigureS5_.png",sep=""), bg = "white",  width = 1500, height = 1000, res = 200)
par(mfrow = c(2,2),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
# (a)
xzoom = max(Rs_testing_VarioEstimation[,2])*1.1
yzoom = max(Rs_testing_VarioEstimation[,3])*1.3
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel )
par(new=TRUE)
plot(Rs_testing_VarioEstimation$Lags, Rs_SGCoSim_sim_VarioEstimation_df[,1], pch = 15, cex = 1.2, col = "darkred",  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel)
par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(Rs_sill_and_nugget-Rs_nugget, Rs_range)
                 , nug = Rs_nugget, col = "Black", lwd = 3, max.dist = N_lags*lag_value )
box()

# (b)
xzoom = max(Rs_testing_VarioEstimation[,2])*1.1
yzoom = max(Rs_testing_VarioEstimation[,3])*1.3
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel )
par(new=TRUE)
plot(Rs_testing_VarioEstimation$Lags, Rs_aclhs_sim_VarioEstimation[,3], pch = 18, cex = 1.5, col = "darkgreen",  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel , ylab = Variolabel)
par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(Rs_sill_and_nugget-Rs_nugget, Rs_range)
                 , nug = Rs_nugget, col = "Black", lwd = 3, max.dist = N_lags*lag_value )
box()

# (c)
xzoom = max(Rs_testing_VarioEstimation[,2])*1.1
yzoom = max(Rs_testing_VarioEstimation[,3])*1.5
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel )
for (i in 1:10) {
  par(new=TRUE)
  plot(Rs_testing_VarioEstimation$Lags, Rs_SGCoSim_sim_VarioEstimation_df[,i], pch = 15, cex = 1.2, col = "darkred",  xlim = c(0,xzoom), ylim=c(0,yzoom),
       xlab = Dislabel ,ylab = Variolabel)
}
par(new=TRUE)
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel )
par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(Rs_sill_and_nugget-Rs_nugget, Rs_range)
                 , nug = Rs_nugget, col = "Black", lwd = 3, max.dist = N_lags*lag_value )
box()

# (d)
xzoom = max(Rs_testing_VarioEstimation[,2])*1.1
yzoom = max(Rs_testing_VarioEstimation[,3])*1.5
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel )
for (i in 1:10) {
  par(new=TRUE)
  plot(Rs_testing_VarioEstimation$Lags, Rs_aclhs_sim_VarioEstimation_df[,i], pch = 15, cex = 1.2, col = "darkgreen",  xlim = c(0,xzoom), ylim=c(0,yzoom),
       xlab = Dislabel ,ylab = Variolabel)
}
par(new=TRUE)
plot(Rs_testing_VarioEstimation[,c(2,3)], pch = 1,  xlim = c(0,xzoom), ylim=c(0,yzoom),
     xlab = Dislabel ,ylab = Variolabel )

par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(Rs_sill_and_nugget-Rs_nugget, Rs_range)
                 , nug = Rs_nugget, col = "Black", lwd = 3, max.dist = N_lags*lag_value )
box()
dev.off()


#---------------------------------------------Figure6_scatterplots--------------------------------#
Fn_Temp <- estandarizar(cbind(Temp_testing,Rs_testing))[,1]
Fn_Rs <- estandarizar(cbind(Temp_testing,Rs_testing))[,2]
U_limit <- seq(0,1,by = 0.2)
png(paste(FF_dir,"/FigureS6_.png",sep=""),   bg = "white", width = 1200, height = 1000, res = 150)
par(mfrow = c(2,2),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
#-------------------Figure a----------------#
plot(Temp_testing, Rs_testing, pch = 1, xlab= Templabel, ylab = Rslabel,  
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(Rs_testing), max(Rs_testing)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_testing, Rs_testing, pch = 1, xlab= Templabel, ylab = Rslabel, 
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(Rs_testing), max(Rs_testing)))
par(new=TRUE)
plot(Temp_testing, sim_backTrans_Rs_nsim[-aclhs_sub50,1], pch = 15, xlab= Templabel, ylab = Rslabel, 
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "darkred",
     ylim = c(min(Rs_testing), max(Rs_testing)))
box()

#----------------- Figure b----------------#
plot(Temp_testing, Rs_testing, pch = 1, xlab= Templabel, ylab = Rslabel,  
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(Rs_testing), max(Rs_testing)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_testing, Rs_testing, pch = 1, xlab= Templabel, ylab = Rslabel, 
     xlim = c(min(Temp_testing), max(Temp_testing)),  col = "lightgray",
     ylim = c(min(Rs_testing), max(Rs_testing)))
# # abline(v=Temp_limit, col= 'green', lty =2, lwd=1)
# # abline(h=Rs_limit, col= 'green', lty =2, lwd=1)
par(new=TRUE)
plot(Temp_testing, Rs_aclhs_nsim[-aclhs_sub50,1],  xlab= Templabel, ylab = Rslabel,
     pch = 18, col = "darkgreen", cex = 1.2,
     xlim = c(min(Temp_testing), max(Temp_testing)),  ylim = c(min(Rs_testing), max(Rs_testing)))
box()

#----------------- Figure c----------------#
plot(Temp_testing, Rs_testing, pch = 1, xlab= Templabel, ylab = Rslabel,  
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(Rs_testing), max(Rs_testing)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_testing, Rs_testing, pch = 1, xlab= Templabel, ylab = Rslabel, 
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(Rs_testing), max(Rs_testing)))
for (i in 1:10) {
  par(new=TRUE)
  plot(Temp_testing, sim_backTrans_Rs_nsim[-aclhs_sub50,i],  xlab= Templabel, ylab = Rslabel,
       pch = 15, col = "darkred", cex = 1.2,
       xlim = c(min(Temp_testing), max(Temp_testing)),  ylim = c(min(Rs_testing), max(Rs_testing)))
}
box()

#----------------- Figure d----------------#
plot(Temp_testing, Rs_testing, pch = 1, xlab= Templabel, ylab = Rslabel,  
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(Rs_testing), max(Rs_testing)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_testing, Rs_testing, pch = 1, xlab= Templabel, ylab = Rslabel, 
     xlim = c(min(Temp_testing), max(Temp_testing)), col = "lightgray",
     ylim = c(min(Rs_testing), max(Rs_testing)))
for (i in 1:10) {
  par(new=TRUE)
  plot(Temp_testing, Rs_aclhs_nsim[-aclhs_sub50,i],  xlab= Templabel, ylab = Rslabel,
       pch = 18, col = "darkgreen", cex = 1.5,
       xlim = c(min(Temp_testing), max(Temp_testing)),  ylim = c(min(Rs_testing), max(Rs_testing)))
}
box()

dev.off()


#--------------------------------------Tables--------------------------------#
# Statistical Properties
# Calculate statistical properties for SGCoSim and CopCoSim predictions
# Kolmogorov-Smirnov (KS) Tests
# Perform KS tests to compare the distributions of predictions against the observed data
ks_test_SGCoSim_Rs_sim <- ks.test(sim_backTrans_Rs_nsim[-aclhs_sub50,1], Rs_testing, conf.level = 0.95)
ks_test_aclhs_Rs_sim <- ks.test(as.numeric(Rs_aclhs_nsim[-aclhs_sub50,1]), Rs_testing, conf.level = 0.95)
ks_test_SGCoSim_Rs_sim
ks_test_aclhs_Rs_sim

# Calculate and display correlations between Temp and CO2 using different methods
# Data
(corP_Temp_Rs_testing <- round(cor(Temp_testing,Rs_testing, method = "pearson"),3))
(corS_Temp_Rs_testing <- round(cor(Temp_testing,Rs_testing, method = "spearman"),3))
(corK_Temp_Rs_testing <- round(cor(Temp_testing,Rs_testing, method = "kendall"),3))
# Correlations for SGCoSim predictions
(corP_Temp_Rs_SGCoSim_pred_testing <- round(cor(Temp_testing,sim_backTrans_Rs_nsim[-aclhs_sub50,1], method = "pearson"),3))
(corS_Temp_Rs_SGCoSim_pred_testing <- round(cor(Temp_testing,sim_backTrans_Rs_nsim[-aclhs_sub50,1], method = "spearman"),3))
(corK_Temp_Rs_SGCoSim_pred_testing <- round(cor(Temp_testing,sim_backTrans_Rs_nsim[-aclhs_sub50,1], method = "kendall"),3))
# Correlations for CopCoSim predictions
(corP_Temp_Rs_aclhs_pred_testing <- round(cor(Temp_testing,Rs_aclhs_nsim[-aclhs_sub50,1], method = "pearson"),3))
(corS_Temp_Rs_aclhs_pred_testing <- round(cor(Temp_testing,Rs_aclhs_nsim[-aclhs_sub50,1], method = "spearman"),3))
(corK_Temp_Rs_aclhs_pred_testing <- round(cor(Temp_testing,Rs_aclhs_nsim[-aclhs_sub50,1], method = "kendall"),3))

# Absolute differences between original and predicted correlations
abs(corP_Temp_Rs_SGCoSim_pred_testing-corP_Temp_Rs_testing)
abs(corS_Temp_Rs_SGCoSim_pred_testing-corS_Temp_Rs_testing)
abs(corK_Temp_Rs_SGCoSim_pred_testing-corK_Temp_Rs_testing)
abs(corP_Temp_Rs_aclhs_pred_testing-corP_Temp_Rs_testing)
abs(corS_Temp_Rs_aclhs_pred_testing-corS_Temp_Rs_testing)
abs(corK_Temp_Rs_aclhs_pred_testing-corK_Temp_Rs_testing)

# Relative Errors
# Calculate relative error (%) for SGCoSim and CopCoSim predictions
(sum(abs(Rs_testing-as.numeric(sim_backTrans_Rs_nsim[-aclhs_sub50,1])))/sum(Rs))*100
(sum(abs(Rs_testing-as.numeric(Rs_aclhs_nsim[-aclhs_sub50,1])))/sum(Rs))*100

# Absolute Errors
# Total absolute errors for SGCoSim and CopCoSim predictions
sum(abs(Rs_testing-as.numeric(sim_backTrans_Rs_nsim[-aclhs_sub50,1])))
sum(abs(Rs_testing-as.numeric(Rs_aclhs_nsim[-aclhs_sub50,1])))

# Error Ratio
# Ratio of SGCoSim absolute error to aclhs absolute error
sum(abs(Rs_testing-as.numeric(sim_backTrans_Rs_nsim[-aclhs_sub50,1])))/sum(abs(Rs_testing-as.numeric(Rs_aclhs_nsim[-aclhs_sub50,1])))

# Variogram Errors
# Calculate total absolute errors in variogram estimation
sum(abs(Rs_SGCoSim_sim_VarioEstimation_df[,1]-Rs_testing_VarioEstimation$Semivarianzas))
sum(abs(Rs_aclhs_sim_VarioEstimation$Semivarianzas-Rs_testing_VarioEstimation$Semivarianzas))

# Ratio of variogram errors between SGCoSim and CopCoSim
sum(abs(Rs_SGCoSim_sim_VarioEstimation_df[,1]-Rs_testing_VarioEstimation$Semivarianzas))/sum(abs(Rs_aclhs_sim_VarioEstimation$Semivarianzas-Rs_testing_VarioEstimation$Semivarianzas))

