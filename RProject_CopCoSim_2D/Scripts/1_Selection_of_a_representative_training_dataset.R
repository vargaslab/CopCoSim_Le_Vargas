####   (1) Selection of a representative training dataset   ####
#--------------------------------------------------------------#
#                   Data Manipulation                          #
#--------------------------------------------------------------#
# root_dir- root work directory
root_dir<-getwd()

# Creates a folder to store results for a case
dir.create(paste(getwd(),"/Results/spatial_2D", sep=""))
result_dir<-paste(root_dir,"/Results/spatial_2D",sep="")

# Creates a folder to store results for the Exploratory Data Analysis (EDA)
dir.create(paste(getwd(),"/Results/spatial_2D/EDA", sep=""))
aed_dir<-paste(result_dir,"/EDA",sep="")

# Reading Data File in ASCII format space-separated (.txt)
# Uncomment and use if needed.
# Data_File <- read.table(file=file.choose(), header=TRUE, na.strings="-999.25")

# Reading Data File in ASCII format comma separated (.csv)
# -999.25 for non available values
Data_Frame <- read.csv(file=file.choose(),header=T,na.strings="-999.25")

# Extract variables
X<-Data_Frame[,1]
Y<-Data_Frame[,2]
Temp<-Data_Frame[,3]   
Rs<-Data_Frame[,4]   

# Label definitions
Xlabel <- expression(bold("X [km]")) 
Ylabel <- expression(bold("Y [km]")) 
Rslabel <- expression(bold(paste(Rs ~ "["~ g ~ C ~ m^-2 ~"]"))) 
Templabel <- expression(bold("Temp [°C]"))
Dislabel <- expression(bold("Distance [km]"))
  
#--------------------------------------------------------------#
#               Exploratory Data Analysis                      #
#--------------------------------------------------------------#
###------------ Univariate Data Analysis---------------------###

# Basic Statistics
X_Stat<-Estadisticas(X)
Y_Stat<-Estadisticas(Y)
Rs_Stat<-Estadisticas(Rs)
Temp_Stat<-Estadisticas(Temp)
n_bins = nclass.Sturges(Rs)

#--------------Rs data-----------------#
# Histogram and Boxplot
# Save plot as image in png format
png(paste(aed_dir,"/Rs_HistBoxPlotCounts.png",sep=""), bg = "white", width = 1500, height = 1500, res = 250)
HistBoxplot(x=Rs, mean = Rs_Stat[5,2], median = Rs_Stat[4,2], main = "",  
            xlab = Rslabel, ylab = expression(bold("Absolute frequency [count]")), AbsFreq = TRUE, PercentFreq = FALSE,
            nbin = n_bins)
dev.off()
png(paste(aed_dir,"/Rs_HistBoxPlotFreq.png",sep=""), bg = "white",  width = 1500, height = 1500, res = 250)
histf_Rs <- HistBoxplot(x=Rs, mean = Rs_Stat[5,2], median = Rs_Stat[4,2], main ="", 
            xlab = Rslabel, ylab = expression(bold("Relative frequency [%]")), AbsFreq = FALSE, PercentFreq = TRUE,
            nbin =n_bins)
dev.off()

#-----------Temp data-----------------#
# Basic Statistics
png(paste(aed_dir,"/Temp_HistBoxPlotFreq.png",sep=""), bg = "white",  width = 1500, height = 1500, res = 250)
HistBoxplot(x=Temp, mean = Temp_Stat[5,2], median = Temp_Stat[4,2], main ="", 
            xlab = Templabel, ylab = expression(bold("Relative frequency [%]")), AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = n_bins)
dev.off()
png(paste(aed_dir,"/Temp_HistBoxPlotCounts.png",sep=""), bg = "white",  width = 1500, height = 1500, res = 250)
histf_Temp <- HistBoxplot(x=Temp, mean = Temp_Stat[5,2], median = Temp_Stat[4,2], main ="", 
            xlab = Templabel, ylab = expression(bold("Absolute frequency [count]")), AbsFreq = TRUE, PercentFreq = FALSE,
            nbin = n_bins)
dev.off()

###------------ Bivariate Data Analysis---------------------###
# Scatterplot with linear correlation coefficient
# Temp is the independent variable (x-axis)
# Rs is the dependent variable (y-axis)
png(paste(aed_dir,"/Rs-Temp_ScatterPlot.png",sep=""), bg = "white",  width = 1500, height = 1500, res = 220)
ScatterPlot(Temp, Rs,  n_bins, 
            Ymin = Rs_Stat[2,2], Ymax = Rs_Stat[7,2], 
            Xmin = Temp_Stat[2,2], Xmax = Temp_Stat[7,2], 
            YLAB = Rslabel, XLAB = Templabel)
dev.off()

#--------------------------------------------------------------#
#                   Variography Analysis                       #
#--------------------------------------------------------------#
# Define the root working directory
root_dir<-getwd()

# Creates a folder to store results for a case
# dir.create(paste(getwd(),"/Results/spatial_2D", sep=""))
# result_dir<-paste(root_dir,"/Results/spatial_2D",sep="")
# Creates a folder to store results for Variogram Analysis (VA)
dir.create(paste(getwd(),"/Results/spatial_2D/VA", sep=""))
VA_dir<-paste(root_dir,"/Results/spatial_2D/VA",sep="")

#--------------------------------------------------------------#
#                     Spatial Distribution                     #
#--------------------------------------------------------------#
png(paste(VA_dir,"/Rs_Spatial_Distr.png",sep=""), 
    bg = "white", width = 1500, height = 1000, res = 100)
DEspacial(X, Y, Rs, 
          Xlabel , Ylabel, Rslabel, 'Rs Spatial Distribution')
dev.off()

# Estimation of the experimental variogram
X_rng<-X_Stat[8,2]
Y_rng<-Y_Stat[8,2]
N_lags<- 35
lag_value <- sqrt(X_rng*X_rng+Y_rng*Y_rng)/(2*N_lags)
DistMin<-min(dist(Data_Frame[,1:2])) # Minimum distance in data
DistMax<-max(dist(Data_Frame[,1:2])) # Maximum distance in data
lag_value<- DistMin #max((DistMax/2)/N_lags, DistMin)
Rs_VarioEstimation<-Variograma(X, Y, 
                               Rs, 0, 90, N_lags, lag_value, 1, "", "Distance [m]")

#Variogram models (1- exponential, 2- spherical, 3- gaussian)
N_lags<- 35
lag_value <- lag_value
Rs_vario_model<- 2 
Rs_nugget<- 13000 
Rs_sill_and_nugget<- 125000
Rs_range <- 3000
png(paste(VA_dir,"/Rs_Variogram.png",sep=""), bg = "white",  width = 1500, height = 1000, res = 200)
par(mfrow = c(1,1),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
plot(Rs_VarioEstimation[,c(2,3)], xlim = c(0,1.1*max(Rs_VarioEstimation$Lags)),
     pch = 19, ylim= c(0,1.1*max(Rs_VarioEstimation$Semivarianzas)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Rs_VarioEstimation[,c(2,3)], xlim = c(0,1.1*max(Rs_VarioEstimation$Lags)), 
     pch = 19, ylim= c(0,1.1*max(Rs_VarioEstimation$Semivarianzas)))
par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(Rs_sill_and_nugget-Rs_nugget, Rs_range)
                 , nug = Rs_nugget, col = "Black", lwd = 3, max.dist = N_lags*lag_value )
legend("topleft",legend=c("Empirical variogram", "Variogram model") # ,
       ,col=c("black", "black"),lty = c(1,1),lwd = c(NA, 3), pch = c(1, NA), box.lty=0) # , title ="Legend"
box()
dev.off()

# Estimation of the experimental variogram
X_rng<-X_Stat[8,2]
Y_rng<-Y_Stat[8,2]
N_lags<- 35
lag_value <- sqrt(X_rng*X_rng+Y_rng*Y_rng)/(2*N_lags)
DistMin<-min(dist(Data_Frame[,1:2])) # Minimum distance in data
DistMax<-max(dist(Data_Frame[,1:2])) # Maximum distance in data
lag_value<- DistMin                  #max((DistMax/2)/N_lags, DistMin)
Temp_VarioEstimation<-Variograma(X, Y, 
                                 Temp, 0, 90, N_lags, lag_value, 1, "", "Distance [m]")

#Variogram models (1- exponential, 2- spherical, 3- gaussian)
N_lags<- 35
lag_value <- lag_value
Temp_vario_model<- 2 
Temp_nugget<- 0.1
Temp_sill_and_nugget<- 40
Temp_range <- 3000
png(paste(VA_dir,"/Temp_Variogram.png",sep=""), bg = "white",  width = 1500, height = 1000, res = 200)
par(mfrow = c(1,1),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
plot(Temp_VarioEstimation[,c(2,3)], xlim = c(0,1.1*max(Temp_VarioEstimation$Lags)),
     pch = 19, ylim= c(0,1.1*max(Temp_VarioEstimation$Semivarianzas)))
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(Temp_VarioEstimation[,c(2,3)], xlim = c(0,1.1*max(Temp_VarioEstimation$Lags)), 
     pch = 19, ylim= c(0,1.1*max(Temp_VarioEstimation$Semivarianzas)))
par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(Temp_sill_and_nugget-Temp_nugget, Temp_range)
                 , nug = Temp_nugget, col = "Black", lwd = 3, max.dist = N_lags*lag_value )
legend("topleft",legend=c("Empirical variogram", "Variogram model") # ,
       ,col=c("black", "black"),lty = c(1,1),lwd = c(NA, 3), pch = c(1, NA), box.lty=0) # , title ="Legend"
box()
dev.off()


#------------------------------------ Applying acLHS---------------------------#
# Sampling number for training data
n_sub = 150
Fn_limit <- seq(0,1,length.out = n_sub+1)
Temp_limit <- quantile(Temp,Fn_limit)
Rs_limit <- quantile(Rs,Fn_limit)
X_limit <- quantile(X,Fn_limit)
Y_limit <- quantile(Y,Fn_limit)
# OF1
FuncO1  <- function(var_sub) {
  pos_sub <- NULL
  for (i in 1:n_sub) {
    pos_sub <- rbind(pos_sub,which.min(abs(Rs-var_sub[i])))
  }
  Temp_sub <- Temp[pos_sub]
  hist_Temp_sub <- hist(Temp_sub, breaks = Temp_limit, plot = FALSE)$counts
  frecuencia <- rep(1,n_sub)
  return (sum(abs(hist_Temp_sub-frecuencia))) 
}             

corP_Temp_Rs <-  cor(Temp,Rs, method = "pearson")
corS_Temp_Rs <- cor(Temp,Rs, method = "spearman")
corK_Temp_Rs <- cor(Temp,Rs, method = "kendall")
# OF2
FuncO2  <- function(var_sub) {
  pos_sub <- NULL
  for (i in 1:n_sub) {
    pos_sub <- rbind(pos_sub,which.min(abs(Rs-var_sub[i])))
  }
  corP_Temp_Vsub <- cor(Temp[pos_sub],Rs[pos_sub], method = "pearson")
  corS_Temp_Vsub <- cor(Temp[pos_sub],Rs[pos_sub], method = "spearman")
  corK_Temp_Vsub <- cor(Temp[pos_sub],Rs[pos_sub], method = "kendall")
  return (abs(corP_Temp_Vsub-corP_Temp_Rs) + abs(corS_Temp_Vsub-corS_Temp_Rs) + abs(corK_Temp_Vsub-corK_Temp_Rs)) 
}

#OF3
FuncO3 <- function(var_sub) {
  N_lags<- 35
  lag_value <- DistMin
  pos_sub <- NULL
  for (i in 1:n_sub) {
    pos_sub <- rbind(pos_sub,which.min(abs(Rs-var_sub[i])))
  }
  X_sub <- X[pos_sub]
  Y_sub <- Y[pos_sub]
  Temp_sub <- Temp[pos_sub]
  Vario_var_sub <- variog(as.geodata(cbind(X_sub, Y_sub, var_sub), 
                                     coords.col = 1:2, data.col = 3),  breaks = c(seq(lag_value/2, lag_value * (N_lags+1), lag_value)),
                          trend = "cte", lambda = 1, estimator.type = "classical", nugget.tolerance = 0, direction = 0, 
                          tolerance = 90, unit.angle = "degrees", pairs.min = 1)
  Temp_Vario_var_sub <- variog(as.geodata(cbind(X_sub, Y_sub, Temp_sub), 
                                          coords.col = 1:2, data.col = 3),  breaks = c(seq(lag_value/2, lag_value * (N_lags+1), lag_value)),
                               trend = "cte", lambda = 1, estimator.type = "classical", nugget.tolerance = 0, direction = 0, 
                               tolerance = 90, unit.angle = "degrees", pairs.min = 1)
  return (sum(abs(Vario_var_sub$v-Rs_VarioEstimation[,3])+ abs(Temp_Vario_var_sub$v-Temp_VarioEstimation[,3]))) # # initial = 11.20084
}

# Combined Objective Function: Weighted sum of OF1, OF2, and OF3
w1 = 10; w2 = 1000 ; w3 = 0.001 
FuncConj  <- function(var_sub) {
  total =  w1*FuncO1(var_sub) + w2*FuncO2(var_sub) + w3*FuncO3(var_sub) 
  return (total)
}

# Optimization setup using DEoptim
Fn_opt_sub <- seq(1/n_sub,1,length.out = n_sub)
Rs_optimal_sub <- unname(quantile(Rs,Fn_opt_sub)) 
Rs_order <- Rs[order(Rs)]
pos_otp_order <- NULL
for (i in 1:n_sub) {
  pos_otp_order <- rbind(pos_otp_order,which.min(abs(Rs_order-Rs_optimal_sub[i])))
}
tol_sub <-  round(length(Rs)/n_sub)
Pop_Inicial <- matrix(data=NA,nrow=tol_sub,ncol=n_sub)
for (i in 1:(n_sub)) {
  for (j in 1:tol_sub) {
    pos <- pos_otp_order[i]
    Pop_Inicial[j,i] <- Rs_order[pos-j]
  }
}
Fn_sub_limit <- seq(0,1,1/n_sub)
Rs_sub_limit <- unname(quantile(Rs, probs = Fn_sub_limit))
subvector_min = Rs_sub_limit[-(n_sub+1)]
subvector_max = Rs_sub_limit[-(1)]
system.time(aclhs_sub_outDEoptim_Rs <- DEoptim(fn=FuncConj, lower= subvector_min,
                                               upper= subvector_max,
                                               control = DEoptim.control(VTR = 0.000001,strategy = 3, 
                                                                         itermax =20000, reltol = 1e-8,
                                                                         CR = 0.5, F = 0.8, NP= nrow(Pop_Inicial),
                                                                         initialpop = Pop_Inicial))) #, 
# Extracting results
aclhs_sub_outDEoptim_Rs$optim$bestval 
Ite <- seq(1,length(aclhs_sub_outDEoptim_Rs$member$bestvalit),1)
plot(Ite,aclhs_sub_outDEoptim_Rs$member$bestvalit, "l", xlab ="Iterations", ylab ="Objective Function")
var_sub <- as.numeric(aclhs_sub_outDEoptim_Rs$optim$bestmem) 

# Statistics for the selected samples
Estadisticas(var_sub)

# Extracting the subset position
pos_sub_Rs <- NULL
for (i in 1:n_sub) {
  pos_sub_Rs <- rbind(pos_sub_Rs,which.min(abs(Rs-var_sub[i])))
}

# Extracting the subset
aclhs_sub50 <- pos_sub_Rs
X_aclhs_sub50 <- X[aclhs_sub50]
Y_aclhs_sub50 <- Y[aclhs_sub50]
Temp_aclhs_sub50 <- Temp[aclhs_sub50]
Rs_aclhs_sub50 <- Rs[aclhs_sub50]

# Statistics for the selected samples
Rs_aclhs_sub50_Stat <- Estadisticas(Rs_aclhs_sub50)
Temp_aclhs_sub50_Stat <- Estadisticas(Temp_aclhs_sub50)
# aclhs_Sample <- cbind(X_aclhs_sub50,Y_aclhs_sub50,Temp_aclhs_sub50,Rs_aclhs_sub50)
# write.csv(aclhs_Sample , file = paste(SM_dir,"/aclhs_Sample.csv",sep=""))

# Optimization Process
plot(Ite,aclhs_sub_outDEoptim_Rs$member$bestvalit,
     "l", xlab = expression(bold("Iterations")), ylab = expression(bold("Objective Function")))

#--------------Comparison between the subset and the input data----------------#
# Statistical Properties
cbind(Rs_Stat,Rs_fixed_sub50_Stat,Rs_clhs_sub50_Stat,Rs_aclhs_sub50_Stat)
cbind(Temp_Stat,Temp_fixed_sub50_Stat,Temp_clhs_sub50_Stat,Temp_aclhs_sub50_Stat)

# Kolmogorov Smirnov test
ks_test_aclhs_Rs <- ks.test(Rs_aclhs_sub50, Rs, conf.level = 0.95)
ks_test_aclhs_Rs
ks_test_aclhs_Temp <- ks.test(Temp_aclhs_sub50, Temp, conf.level = 0.95)
ks_test_aclhs_Temp

# Calculate initial correlation values between Temp and CO2
(corP_Temp_Rs <- round(cor(Temp,Rs, method = "pearson"),1))
(corS_Temp_Rs <- round(cor(Temp,Rs, method = "spearman"),1))
(corK_Temp_Rs <- round(cor(Temp,Rs, method = "kendall"),1))

# Calculate correlation values between Temp and CO2 from subset
(corP_Temp_Rs_aclhs_sub50 <- round(cor(Temp_aclhs_sub50,Rs_aclhs_sub50, method = "pearson"),1))
(corS_Temp_Rs_aclhs_sub50 <- round(cor(Temp_aclhs_sub50,Rs_aclhs_sub50, method = "spearman"),1))
(corK_Temp_Rs_aclhs_sub50 <- round(cor(Temp_aclhs_sub50,Rs_aclhs_sub50, method = "kendall"),1))


# Estimation of the experimental variogram for RS from subset
N_lags<- 35
lag_value<- DistMin 
Rs_aclhs_sub50_VarioEstimation <-Variograma(X_aclhs_sub50, Y_aclhs_sub50, 
                               Rs_aclhs_sub50, 0, 90, N_lags, lag_value, 1, "", "Distance [m]")

# Calculating the total sum of semivariograms
sum(Rs_aclhs_sub50_VarioEstimation[,3])
sum(Rs_VarioEstimation[,3])
