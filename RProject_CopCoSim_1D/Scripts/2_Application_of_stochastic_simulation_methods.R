###              (2) Application of stochastic simulation methods            ###

#------------------------------------------------------------------------------#
# 1. Implementation of the Sequential Gaussian Co-Simulation (SGCoSim) Method  #
#------------------------------------------------------------------------------#
time_SGCoSim <- system.time({
# Normalize variables using normal score transformation using the training data
# Temperature normalization
Normalized_Temp_aclhs_sub48 <- nscore(Temp_aclhs_sub48) 
# CO2 normalization
Normalized_CO2_aclhs_sub48 <- nscore(CO2_aclhs_sub48)

# Definite an imaginary coordinate Y
Y_aclhs_sub48 <- rep(0, length(Time_aclhs_sub48))

# Variogram model parameters for normalized variables:
# Temp
Normalized_Temp_vario_model<- 2                # Spherical model
Normalized_Temp_nugget<- 0.01                  # Nugget effect
Normalized_Temp_sill_minus_nugget<- 0.15-0.01  # Sill minus nugget
Normalized_Temp_rank <- 5                      # Range parameter
# CO2
Normalized_CO2_vario_model<- 2                  # Spherical model
Normalized_CO2_nugget<- 0.01                    # Nugget effect
Normalized_CO2_sill_minus_nugget<- 0.15-0.01    # Sill minus nugget
Normalized_CO2_rank <- 5                        # Range parameter

# Cross-variogram: Temp-Rs
Normalized_Temp_Normalized_CO2_vario_model<- 2             # Spherical model
Normalized_Temp_Normalized_CO2_nugget<- 0                  # Nugget effect
Normalized_Temp_Normalized_CO2_sill_minus_nugget<- 0.06-0  # Sill minus nugget
Normalized_Temp_Normalized_CO2_rank <- 5                   # Range parameter

# Variogram fitting for normalized temperature
N_lags <- 8         # Number of lags
lag_value <- 1      # Lag increment
Normalized_Temp_aclhs_sub48_vario_model<- 2
Normalized_Temp_aclhs_sub48_nugget<- 0.01
Normalized_Temp_aclhs_sub48_sill_and_nugget<- 0.15
Normalized_Temp_aclhs_sub48_rank <- 5
# Fit variogram model visually
Normalized_Temp_aclhs_sub48_EyeModelVarioFit<-EyeModel(Time_aclhs_sub48, Y_aclhs_sub48, 
                                                       Normalized_Temp_aclhs_sub48$nscore, 0, 90, N_lags, lag_value, 1, 
                                                       Normalized_Temp_aclhs_sub48_vario_model, Normalized_Temp_aclhs_sub48_nugget, Normalized_Temp_aclhs_sub48_sill_and_nugget, Normalized_Temp_aclhs_sub48_rank,
                                                       "")

# Variogram fitting for normalized CO2
N_lags <- 8        # Number of lags
lag_value <- 1     # Lag increment
Normalized_CO2_aclhs_sub48_vario_model<- 2
Normalized_CO2_aclhs_sub48_nugget<- 0.01
Normalized_CO2_aclhs_sub48_sill_and_nugget<- 0.15
Normalized_CO2_aclhs_sub48_rank <- 5
# Fit variogram model visually
Normalized_CO2_aclhs_sub48_EyeModelVarioFit<-EyeModel(Time_aclhs_sub48, Y_aclhs_sub48, 
                                                     Normalized_CO2_aclhs_sub48$nscore, 0, 90, N_lags, lag_value, 1, 
                                                     Normalized_CO2_aclhs_sub48_vario_model, Normalized_CO2_aclhs_sub48_nugget, Normalized_CO2_aclhs_sub48_sill_and_nugget, Normalized_CO2_aclhs_sub48_rank,
                                                     "")


# Cross-variogram fitting
par(mfrow = c(1,1),mar = c(4, 6, 2, 2), cex.lab= 1.2, cex.axis = 1.2)
Cross_EyeModelVarioFit <- ModelVariogram(Time_aclhs_sub48, Y_aclhs_sub48, 
               Normalized_Temp_aclhs_sub48$nscore, Normalized_CO2_aclhs_sub48$nscore, 
               N_lags, lag_value, 0, 90, Xlabel = Timelabel,
               Normalized_Temp_Normalized_CO2_vario_model, 
               Normalized_Temp_sill_minus_nugget, Normalized_CO2_sill_minus_nugget, Normalized_Temp_Normalized_CO2_sill_minus_nugget, 
               Normalized_Temp_nugget, Normalized_CO2_nugget, Normalized_Temp_Normalized_CO2_nugget, Normalized_Temp_Normalized_CO2_rank, 
               'N1-score', 'N2-score', 'CrossVariogram' )



# Ensure positive definiteness of variogram model
NugetMatrix <- matrix(c(Normalized_CO2_nugget,
                        Normalized_Temp_Normalized_CO2_nugget,
                        Normalized_Temp_Normalized_CO2_nugget,
                        Normalized_Temp_nugget), ncol = 2)
det(NugetMatrix)
SemiVariMatrix1 <- c(Normalized_CO2_sill_minus_nugget,
                     Normalized_Temp_Normalized_CO2_sill_minus_nugget,
                     Normalized_Temp_Normalized_CO2_sill_minus_nugget,
                     Normalized_Temp_sill_minus_nugget)
SillMatrix <- matrix(SemiVariMatrix1, ncol = 2)
Sill_NuggetMatrix <- SillMatrix - NugetMatrix
# which(Sill_NuggetMatrix < 0) 
# det(SillMatrix)
if (any(Sill_NuggetMatrix < 0)) {
  stop("Sill-Nugget matrix contains negative values.")
}
det(SillMatrix)


# Data frame with coordinates (x, y) and two variables (var1, var2)
data <- data.frame(
  x = Time_aclhs_sub48,                
  y = rep(0,length(Time_aclhs_sub48)), 
  var1 = Normalized_Temp_aclhs_sub48$nscore,             
  var2 = Normalized_CO2_aclhs_sub48$nscore              
)
coordinates(data) <- ~x + y


# Define variogram models
# Variogram model for var1
vgm_var1 <- vgm(psill = 0.15, model = "Sph", range = 5, nugget = 0.01)
# Variogram model for var2
vgm_var2 <- vgm(psill = 0.15, model = "Sph", range = 5, nugget = 0.01)
# Cross-variogram model
vgm_cross <- vgm(psill = 0.06, model = "Sph", range = 5, nugget = 0)


# Create the gstat object and add variogram models
g <- gstat(NULL, id = "var1", formula = var1 ~ 1, data = data, model = vgm_var1)
g <- gstat(g, id = "var2", formula = var2 ~ 1, data = data, model = vgm_var2)
g <- gstat(g, id = c("var1", "var2"), model = vgm_cross)

# Conditional co-simulation
Normalized_Temp <- nscore(Temp)
grid <-  data.frame(
  x = Time,       
  y = rep(0, length(Time))
  ,var1 = Normalized_Temp$nscore
)
coordinates(grid) <- ~x + y


# Perform conditional cosimulation
Cond_SGCoSim <- predict(g, newdata = grid, nsim = 10)

# Convert the results to a data frame
Cond_SGCoSim_df <- as.data.frame(Cond_SGCoSim)

# Back-transform 
sim_backTrans_CO2_nsim = NULL
for (i in 13:22) {
  sim_back = backtr(Cond_SGCoSim_df[,i], Normalized_CO2_aclhs_sub48, tails='none', draw=TRUE)
  sim_backTrans_CO2_nsim  = cbind(sim_backTrans_CO2_nsim, sim_back)
}

# Verify the conditional or training points
cbind(CO2_aclhs_sub48,sim_backTrans_CO2_nsim[aclhs_sub48,])

# UQ (uncertainty quantification)
UQ_min <- apply(sim_backTrans_CO2_nsim, 1, min)
UQ_max <- apply(sim_backTrans_CO2_nsim, 1, max)
#UQ_max_min <- UQ_max-UQ_min
UQ_max_min1 <- abs(UQ_max-CO2)+abs(CO2-UQ_min)
#sum(UQ_max_min)
sum(UQ_max_min1)

})

print(time_SGCoSim)
# user  system elapsed 
# 22.007  16.936   5.594

#------------------------------------------------------------------------------#
# 2. Implementation of the Copula-based Co-Simulation (CopCoSim) Method        #
#------------------------------------------------------------------------------#
time_CopCoSim <- system.time({
#------------------Simulations of CO2 conditioned to Temp----------------------#
# Prepare training data for modeling and simulating using Bernstein Copula
aclhs_Sample <- data.frame(Temp_aclhs_sub48, CO2_aclhs_sub48) 
muestra <- aclhs_Sample
matriz.copem <- genmat.copem(muestra)

# Number of simulations per Temp point
m <- 10 
Temp_aux = Temp

# Ensure Temp values are within the range of observed or training data
for (i in 1:length(Temp_aux)) {
  if (Temp_aux[i] > max(Temp_aclhs_sub48) ){
    Temp_aux[i] = max(Temp_aclhs_sub48)
  }
  if (Temp_aux[i] < min(Temp_aclhs_sub48) ){
    Temp_aux[i] = min(Temp_aclhs_sub48)
  }
}

# Perform conditional simulation using the Bernstein Copula method
system.time(sim.m_aclhs <- mapply(simula.Bernshtein.condicional, Temp_aux, rep(m, length(Temp_aux))))
# sim.m_aclhs is a matrix with m rows and length(Temp) columns
# be patient... 27 seconds approx

#---------------------Optimization Setup---------------------------------------#
# Number of simulation points
n_sim <- 365
# Fractional limits for quantiles from training data
Fn_limit <- seq(0,1,length.out = n_sub+1)
# Quantile limits for CO2 from training data
CO2_aclhs_sub48_limit <- quantile(CO2_aclhs_sub48,Fn_limit)

# Objective Function 1:
#--------------OF1------------#
FuncO1sim  <- function(var_sim) {
  hist_CO2_sim <- hist(var_sim, breaks = CO2_aclhs_sub48_limit, plot = FALSE)$counts
  freq <- rep(n_sim/n_sub,n_sub)
  return (sum(abs(hist_CO2_sim-freq)))
}

# Pre-compute correlation coefficients for training data
corP_Temp_CO2_aclhs_sub48 <- cor(Temp_aclhs_sub48,CO2_aclhs_sub48, method = "pearson")
corS_Temp_CO2_aclhs_sub48 <- cor(Temp_aclhs_sub48,CO2_aclhs_sub48, method = "spearman")
corK_Temp_CO2_aclhs_sub48 <- cor(Temp_aclhs_sub48,CO2_aclhs_sub48, method = "kendall")

# Objective Function 2:
#------------OF2----------------#
FuncO2sim  <- function(var_sim) {
  corP_Temp_var_sim <- cor(Temp,var_sim, method = "pearson")
  corS_Temp_var_sim <- cor(Temp,var_sim, method = "spearman")
  corK_Temp_var_sim <- cor(Temp,var_sim, method = "kendall")
  return (abs(corP_Temp_var_sim-corP_Temp_CO2_aclhs_sub48) + abs(corS_Temp_var_sim-corS_Temp_CO2_aclhs_sub48) + abs(corK_Temp_var_sim-corK_Temp_CO2_aclhs_sub48)) 
}

# Variogram Model Parameters
#Variogram models (1- exponential, 2- spherical, 3- gaussian)
N_lags<- 8
lag_value <- 1 
CO2_aclhs_sub48_vario_model<- 2 
CO2_aclhs_sub48_nugget<- 0.57 
CO2_aclhs_sub48_sill_and_nugget<- 1.95 
CO2_aclhs_sub48_range <- 6.21

# Determine variogram model type
Modelo1 = CO2_aclhs_sub48_vario_model
if (Modelo1 == 1) {
  ModeloA <- "exponential"
}
if (Modelo1 == 2) {
  ModeloA <- "spherical"
}
if (Modelo1 == 3) {
  ModeloA <- "gaussian"
}

# Compute semi-variogram for training data
Sill = CO2_aclhs_sub48_sill_and_nugget - CO2_aclhs_sub48_nugget
Range = CO2_aclhs_sub48_range
VMod = (CO2_aclhs_sub48_sill_and_nugget) - cov.spatial(seq(lag_value,N_lags*lag_value,lag_value), cov.model = ModeloA, 
                                                       cov.pars = c(Sill, Range))
semi_prior = VMod

# Objective Function 3:
#------------OF3------------#
FunCO3sim <- function(var_sim) {
  N_lags<- 8 
  lag_value <- 1 
  t_sim <- Time
  ti_sim <- rep(0,365)
  Vario_var_sim <- variog(as.geodata(cbind(t_sim, ti_sim, var_sim), 
                                     coords.col = 1:2, data.col = 3), breaks = c(seq(lag_value/2, lag_value * (N_lags+1), lag_value)),
                          trend = "cte", lambda = 1, estimator.type = "classical", nugget.tolerance = 0, direction = 0, 
                          tolerance = 90, unit.angle = "degrees", pairs.min = 1)
  return (sum(c(10,rep(1,N_lags-1))*abs(Vario_var_sim$v-semi_prior))) 
}

# Combined Objective Function
ws1 = 0.2; ws2 = 100; ws3 = 1  # Weights for individual objectives
FuncConjsim  <- function(var_sim) {
  total = ws1*FuncO1sim(var_sim) + ws2*FuncO2sim(var_sim) + ws3*FunCO3sim(var_sim) 
  return (total) 
}


#----------------------Differential Evolution Optimization----------------------#
# Set up bounds for optimization
Pop_Inicial <- as.matrix(sim.m_aclhs)
vector_min =  apply(Pop_Inicial, 2, min)  
vector_max =  apply(Pop_Inicial, 2, max)  
vector_min[aclhs_sub48] = CO2_aclhs_sub48
vector_max[aclhs_sub48] = CO2_aclhs_sub48

# Initialize prediction data frame
CO2_aclhs_pred_df <- NULL

# Perform optimization
for (i in 1:10) {
  system.time(
    aclhs_sim_outDEoptim_CO2 <- DEoptim(fn=FuncConjsim, lower= vector_min,
                                        upper= vector_max,
                                        control = DEoptim.control(VTR = 0.0000001,strategy = 3, 
                                                                  itermax =1000, reltol = 1e-8, 
                                                                  CR = 0.5, F = 0.8, NP= nrow(Pop_Inicial), initialpop = Pop_Inicial)) #, , 
  )
  aclhs_sim_outDEoptim_CO2$optim$bestval
  Ite <- seq(1,length(aclhs_sim_outDEoptim_CO2$member$bestvalit),1)
  plot(Ite,aclhs_sim_outDEoptim_CO2$member$bestvalit, "l", xlab ="Iterations", ylab ="Objective Function")
  var_sim <- as.numeric(aclhs_sim_outDEoptim_CO2$optim$bestmem)
  
  CO2_aclhs_pred_df <- rbind(CO2_aclhs_pred_df,var_sim)
}

#----------------------Uncertainty Quantification-------------------------------#
# Compute uncertainty ranges
CopCoSim_UQ_min <- apply(CO2_aclhs_pred_df, 2, min)
CopCoSim_UQ_max <- apply(CO2_aclhs_pred_df, 2, max)
#CopCoSim_UQ_max_min <- CopCoSim_UQ_max-CopCoSim_UQ_min
CopCoSim_UQ_max_min1 <- abs(CopCoSim_UQ_max-CO2)+abs(CO2-CopCoSim_UQ_min)
#sum(CopCoSim_UQ_max_min)
sum(CopCoSim_UQ_max_min1)

})

print(time_CopCoSim)
# user  system elapsed 
# 922.322  44.993 967.249 