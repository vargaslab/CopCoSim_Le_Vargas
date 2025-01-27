####   (1) Selection of a representative training dataset   ####
#--------------------------------------------------------------#
#                   Data Manipulation                          #
#--------------------------------------------------------------#
# root_dir- root work directory
root_dir<-getwd()

# Create a folder to store results for a case
dir.create(paste(getwd(),"/Results/Time_Series", sep=""))
result_dir<-paste(root_dir,"/Results/Time_Series",sep="")

# Create a folder to store results for the Exploratory Data Analysis (EDA)
dir.create(paste(getwd(),"/Results/Time_Series/EDA", sep=""))
aed_dir<-paste(result_dir,"/EDA",sep="")

# Reading Data File in ASCII format space-separated (.txt)
# Uncomment and use if needed.
# Data_File <- read.table(file=file.choose(), header=TRUE, na.strings="-999.25")

# Reading Data File in ASCII format comma-separated (.csv)
# -999.25 for non available values
Data_Frame <- read.csv(file=file.choose(),header=T,na.strings="-999.25")

# Extract variables
Time<-Data_Frame$Time
Temp<- Data_Frame$Temp
CO2<- Data_Frame$CO2

# Label definitions
Timelabel <- expression(bold("Time [days]")) 
CO2label <- expression(bold(paste(CO[2] ~ efflux ~ "["~μmol ~ m^-2 ~ s^-1~"]"))) 
Templabel <- expression(bold("Temperature [°C]"))
  
#--------------------------------------------------------------#
#               Exploratory Data Analysis                      #
#--------------------------------------------------------------#
###------------ Univariate Data Analysis---------------------###

# Basic Statistics
Time_Stat<-Estadisticas(Time)
CO2_Stat<-Estadisticas(CO2)
Temp_Stat<-Estadisticas(Temp)
n_bins = nclass.Sturges(CO2)


#--------------CO2 data-----------------#
# Histogram and Boxplot
# Save plot as image in png format
png(paste(aed_dir,"/CO2_HistBoxPlotCounts.png",sep=""), bg = "white", width = 1500, height = 1500, res = 250)
HistBoxplot(x=CO2, mean = CO2_Stat[5,2], median = CO2_Stat[4,2], main = "",  
            xlab = CO2label, ylab = expression(bold("Absolute frequency [count]")), AbsFreq = TRUE, PercentFreq = FALSE,
            nbin = n_bins)
dev.off()
png(paste(aed_dir,"/CO2_HistBoxPlotFreq.png",sep=""), bg = "white",  width = 1500, height = 1500, res = 250)
histf_CO2 <- HistBoxplot(x=CO2, mean = CO2_Stat[5,2], median = CO2_Stat[4,2], main ="", 
            xlab = CO2label, ylab = expression(bold("Relative frequency [%]")), AbsFreq = FALSE, PercentFreq = TRUE,
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
# CO2 is the dependent variable (y-axis)
png(paste(aed_dir,"/CO2-Temp_ScatterPlot.png",sep=""), bg = "white",  width = 1500, height = 1500, res = 220)
ScatterPlot(Temp, CO2,  n_bins, 
            Ymin = CO2_Stat[2,2], Ymax = CO2_Stat[7,2], 
            Xmin = Temp_Stat[2,2], Xmax = Temp_Stat[7,2], 
            YLAB = CO2label, XLAB = Templabel)
dev.off()

#--------------------------------------------------------------#
#                   Variogram Analysis                         #
#--------------------------------------------------------------#
# Define the root working directory
root_dir<-getwd()

# Create a folder to store results for  Variogram Analysis (VA)
dir.create(paste(getwd(),"/Results/Time_Series/VA", sep=""))
VA_dir<-paste(root_dir,"/Results/Time_Series/VA",sep="")

#--------------------------------------------------------------#
#                    Temporal Distribution                     #
#--------------------------------------------------------------#
log_list=list(Temp,CO2)
n_logs= length(log_list)
log_xlabel_list=list(Templabel,CO2label)
plot_mean=TRUE
plot_median=TRUE
fontproportion=1.0
png(paste(VA_dir,"/Temp_CO2_Temporal_Distr.png",sep=""), 
    bg = "white", width = 1500, height = 1000, res = 150 )
par(mfrow = c(n_logs,1),mar = c(4, 6, 2, 2), cex.lab= 1.0, cex.axis = 1.0)
for (i in 1:n_logs) {
  p=unlist(log_list[i])
  log_label=unlist(log_xlabel_list[i])
  s=summary(p)
  plot(Time, p,  type = "b", pch = 19,
       ylab = log_label , xlab = Timelabel, bty="o")
  grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
  par(new=TRUE)
  plot(Time, p, type = "b", pch = 19,
       ylab =log_label , xlab = Timelabel, bty="o")
  if (plot_mean) {
    abline(h = s[4], col = "Red")
  }  
  if (plot_median) {
    abline(h = s[3], col = "Blue", lty = 2)
  }
  legend("topleft", legend=c("Mean", "Median"),
         col=c("red", "blue"), lty=1:2, box.lty=0)
  box()
}
dev.off()

#--------------------------------------------------------------#
#               Trend (Stationarity) Analysis                  #
#--------------------------------------------------------------#

# Estimation of the experimental variogram
# CO2 temporal variogram
N_lags<- 8       
lag_value <- 1   
ti = numeric(length(Time))
CO2_VarioEstimation<-Variograma(Time, ti, 
                                CO2, 0, 90, N_lags, lag_value, 1, "", Timelabel)

# Temp temporal variogram
N_lags<- 8       
lag_value <- 1   
ti = numeric(length(Time))
Temp_VarioEstimation<-Variograma(Time, ti, 
                                 Temp, 0, 90, N_lags, lag_value, 1, "", Timelabel) 

#--------------------------------------------------------------#
#       Univariate Variography (Structural)  Modeling          #
#--------------------------------------------------------------#

#Variogram models (1- exponential, 2- spherical, 3- gaussian)
N_lags<- 8
lag_value <- 1 
CO2_vario_model<- 2 
CO2_nugget<- 0.57 
CO2_sill_and_nugget<- 1.95 
CO2_range <- 6.21
png(paste(VA_dir,"/CO2_Variogram.png",sep=""), bg = "white",  width = 1500, height = 1000, res = 200)
par(mfrow = c(1,1),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
plot(CO2_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_VarioEstimation[,3]*1.7)),
     xlab = Timelabel ,ylab = Variolabel)
grid(col = "lightgray", lty = "dashed", lwd = par("lwd"), equilogs = TRUE)
par(new=TRUE)
plot(CO2_VarioEstimation[,c(2,3)], pch = 1, xlim = c(0,9), ylim=c(0,max(CO2_VarioEstimation[,3]*1.7)),
     xlab = Timelabel ,ylab = Variolabel )
par(new=TRUE)
lines.variomodel(cov.model = "sph", cov.pars = c(CO2_sill_and_nugget-CO2_nugget, CO2_range)
                 , nug = CO2_nugget, col = "Black", lwd = 3, max.dist = 1*8)
legend("topleft",legend=c("Empirical variogram", "Variogram model") # ,
       ,col=c("black", "black"),lty = c(1,1),lwd = c(NA, 3), pch = c(1, NA), box.lty=0) # , title ="Legend"
box()
dev.off()


#--------------------------Applying acLHS--------------------------------------#
# Sampling number for training data
n_sub = 48
# Fractional limits for quantiles
Fn_limit <- seq(0,1,length.out = n_sub+1)
# Quantile limits for temperature
Temp_limit <- quantile(Temp,Fn_limit)
# Quantile limits for CO2
CO2_limit <- quantile(CO2,Fn_limit)
# Quantile limits for time
Time_limit <- quantile(Time,Fn_limit)

# Objective Function 1 (OF1): Minimize histogram differences
FuncO1  <- function(var_sub) {
  pos_sub <- NULL
  for (i in 1:n_sub) {
    pos_sub <- rbind(pos_sub,which.min(abs(CO2-var_sub[i])))
  }
  Temp_sub <- Temp[pos_sub]
  hist_Temp_sub <- hist(Temp_sub, breaks = Temp_limit, plot = FALSE)$counts
  frecuencia <- rep(1,n_sub)
  return (sum(abs(hist_Temp_sub-frecuencia))) 
}

# Calculate initial correlation values between Temp and CO2
corP_Temp_CO2 <- round(cor(Temp,CO2, method = "pearson"),3)
corS_Temp_CO2 <- round(cor(Temp,CO2, method = "spearman"),3)
corK_Temp_CO2 <- round(cor(Temp,CO2, method = "kendall"),3)

# Objective Function 2 (OF2): Minimize differences in correlations
FuncO2  <- function(var_sub) {
  pos_sub <- NULL
  for (i in 1:n_sub) {
    pos_sub <- rbind(pos_sub,which.min(abs(CO2-var_sub[i])))
  }
  corP_Temp_Vsub <- round(cor(Temp[pos_sub],CO2[pos_sub], method = "pearson"),3)
  corS_Temp_Vsub <- round(cor(Temp[pos_sub],CO2[pos_sub], method = "spearman"),3)
  corK_Temp_Vsub <- round(cor(Temp[pos_sub],CO2[pos_sub], method = "kendall"),3)
  return (abs(corP_Temp_Vsub-corP_Temp_CO2) + abs(corS_Temp_Vsub-corS_Temp_CO2) + abs(corK_Temp_Vsub-corK_Temp_CO2)) 
}

# Objective Function 3 (OF3): Minimize variogram differences
FuncO3 <- function(var_sub) {
  N_lags<- 8 
  lag_value <- 1 # min(dist(Time))
  pos_sub <- NULL
  for (i in 1:n_sub) {
    pos_sub <- rbind(pos_sub,which.min(abs(CO2-var_sub[i])))
  }
  t_sub <- Time[pos_sub]
  ti_sub <- numeric(length(t_sub))
  Temp_sub <- Temp[pos_sub]
  Vario_var_sub <- variog(as.geodata(cbind(t_sub, ti_sub, var_sub), 
                                     coords.col = 1:2, data.col = 3),  breaks = c(seq(lag_value/2, lag_value * (N_lags+1), lag_value)),
                          trend = "cte", lambda = 1, estimator.type = "classical", nugget.tolerance = 0, direction = 0, 
                          tolerance = 90, unit.angle = "degrees", pairs.min = 1)
  Temp_Vario_var_sub <- variog(as.geodata(cbind(t_sub, ti_sub, Temp_sub), 
                                          coords.col = 1:2, data.col = 3),  breaks = c(seq(lag_value/2, lag_value * (N_lags+1), lag_value)),
                               trend = "cte", lambda = 1, estimator.type = "classical", nugget.tolerance = 0, direction = 0, 
                               tolerance = 90, unit.angle = "degrees", pairs.min = 1)
  return (sum(abs(Vario_var_sub$v-CO2_VarioEstimation[,3]))+ sum(abs(Temp_Vario_var_sub$v-Temp_VarioEstimation[,3]))) # # initial = 11.20084
}

# Combined Objective Function: Weighted sum of OF1, OF2, and OF3
w1 = 0.3; w2 = 100; w3 = 10 
FuncConj  <- function(var_sub) {
  total = w1*FuncO1(var_sub) + w2*FuncO2(var_sub) + w3*FuncO3(var_sub) 
  return (total) 
}

# Optimization setup using DEoptim
Fn_opt_sub <- seq(1/n_sub,1,length.out = n_sub)
CO2_optimal_sub <- unname(quantile(CO2,Fn_opt_sub)) 
CO2_order <- CO2[order(CO2)]
pos_otp_order <- NULL
for (i in 1:n_sub) {
  pos_otp_order <- rbind(pos_otp_order,which.min(abs(CO2_order-CO2_optimal_sub[i])))
}
tol_sub <-  round(length(CO2)/n_sub)
Pop_Inicial <- matrix(data=NA,nrow=tol_sub,ncol=n_sub)
for (i in 1:(n_sub)) {
  for (j in 1:tol_sub) {
    pos <- pos_otp_order[i]
    Pop_Inicial[j,i] <- CO2_order[pos-j]
  }
}
Fn_sub_limit <- seq(0,1,1/n_sub)
CO2_sub_limit <- unname(quantile(CO2, probs = Fn_sub_limit))
subvector_min = CO2_sub_limit[-(n_sub+1)]
subvector_max = CO2_sub_limit[-(1)]
system.time(aclhs_sub_outDEoptim_CO2 <- DEoptim(fn=FuncConj, lower= subvector_min,
                                                upper= subvector_max,
                                                control = DEoptim.control(VTR = 0.000001,strategy = 3, 
                                                                          itermax =5000, reltol = 1e-8, CR = 0.5, F = 0.8,
                                                                          NP= nrow(Pop_Inicial),initialpop = Pop_Inicial))) #, 

# Extracting results
aclhs_sub_outDEoptim_CO2$optim$bestval 
Ite <- seq(1,length(aclhs_sub_outDEoptim_CO2$member$bestvalit),1)
plot(Ite,aclhs_sub_outDEoptim_CO2$member$bestvalit, "l", xlab ="Iterations", ylab ="Objective Function")
var_sub <- as.numeric(aclhs_sub_outDEoptim_CO2$optim$bestmem) 

# Statistics for the selected samples
Estadisticas(var_sub)

# Extracting the subset position
pos_sub_CO2 <- NULL
for (i in 1:n_sub) {
  pos_sub_CO2 <- rbind(pos_sub_CO2,which.min(abs(CO2-var_sub[i])))
}

# Extracting the subset
aclhs_sub48 <- pos_sub_CO2
Time_aclhs_sub48 <- Time[aclhs_sub48]
Temp_aclhs_sub48 <- Temp[aclhs_sub48]
CO2_aclhs_sub48 <- CO2[aclhs_sub48]

# Statistics for the selected samples
CO2_aclhs_sub48_Stat <- Estadisticas(CO2_aclhs_sub48)
Temp_aclhs_sub48_Stat <- Estadisticas(Temp_aclhs_sub48)
# aclhs_Temporal_Sample <- cbind(aclhs_sub48,Time_aclhs_sub48,Temp_aclhs_sub48,CO2_aclhs_sub48)
# Saving the subset if needed
# write.csv(aclhs_Temporal_Sample , file = paste(SM_dir,"/aclhs_Temporal_Sample.csv",sep=""))

# Optimization Process
plot(Ite,aclhs_sub_outDEoptim_CO2$member$bestvalit,
     "l", xlab = expression(bold("Iterations")), ylab = expression(bold("Objective Function")))
#dev.off()


#--------------Comparison between the subset and the input data----------------#
# Statistical Properties
cbind(CO2_Stat,CO2_aclhs_sub48_Stat)  
cbind(Temp_Stat,Temp_aclhs_sub48_Stat) 

# Kolmogorov Smirnov test
ks_test_aclhs_CO2 <- ks.test(CO2_aclhs_sub48, CO2, conf.level = 0.95)
ks_test_aclhs_CO2
ks_test_aclhs_Temp <- ks.test(Temp_aclhs_sub48, Temp, conf.level = 0.95)
ks_test_aclhs_Temp

# Calculate initial correlation values between Temp and CO2
(corP_Temp_CO2 <- round(cor(Temp,CO2, method = "pearson"),1))
(corS_Temp_CO2 <- round(cor(Temp,CO2, method = "spearman"),1))
(corK_Temp_CO2 <- round(cor(Temp,CO2, method = "kendall"),1))
# Calculate correlation values between Temp and CO2 from subset
(corP_Temp_CO2_aclhs_sub48 <- round(cor(Temp_aclhs_sub48,CO2_aclhs_sub48, method = "pearson"),1))
(corS_Temp_CO2_aclhs_sub48 <- round(cor(Temp_aclhs_sub48,CO2_aclhs_sub48, method = "spearman"),1))
(corK_Temp_CO2_aclhs_sub48 <- round(cor(Temp_aclhs_sub48,CO2_aclhs_sub48, method = "kendall"),1))


# Estimation of the experimental variogram for CO2 from subset
N_lags<- 8       
lag_value <- 1   
ti = numeric(length(Time_aclhs_sub48))
CO2_aclhs_sub48_VarioEstimation_ <- Variograma(Time_aclhs_sub48, ti, 
                                  CO2_aclhs_sub48, 0, 90, N_lags, lag_value, 1, "", Timelabel)

# Calculating the total sum of semivariograms
sum(CO2_aclhs_sub48_VarioEstimation[,3])
sum(CO2_VarioEstimation[,3])