###              (2) Application of stochastic simulation methods            ###

#------------------------------------------------------------------------------#
# 1. Implementation of the Sequential Gaussian Co-Simulation (SGCoSim) Method  #
#------------------------------------------------------------------------------#

# Perform Kolmogorov-Smirnov test for normality on CO2 efflux
ks_test_Rs_aclhs_sub50 <- ks.test(Rs_aclhs_sub50, "pnorm", mean = mean(Rs_aclhs_sub50), sd = sd(Rs_aclhs_sub50))
print(ks_test_Rs_aclhs_sub50)

# Perform Kolmogorov-Smirnov test for normality on temperature
ks_test_Temp_aclhs_sub50 <- ks.test(Temp_aclhs_sub50, "pnorm", mean = mean(Temp_aclhs_sub50), sd = sd(Temp_aclhs_sub50))
print(ks_test_Temp_aclhs_sub50)

# Normalize variables using normal score transformation using the training data
# Temperature normalization
Normalized_Temp_aclhs_sub50 <- nscore(Temp_aclhs_sub50)
# CO2 normalization
Normalized_Rs_aclhs_sub50 <- nscore(Rs_aclhs_sub50)

# Variogram model parameters for normalized variables:
# Temp
Normalized_Temp_vario_model<- 2
Normalized_Temp_nugget<- 0.01
Normalized_Temp_sill_minus_nugget<- 1.4-0.01
Normalized_Temp_rank <- 2800
# Rs
Normalized_Rs_vario_model<- 2
Normalized_Rs_nugget<- 0.1
Normalized_Rs_sill_minus_nugget<- 1.4-0.1
Normalized_Rs_rank <- 2800
# Cross-variogram
Normalized_Temp_Normalized_Rs_vario_model<- 2 
Normalized_Temp_Normalized_Rs_nugget<- 0
Normalized_Temp_Normalized_Rs_sill_minus_nugget<- 0.45-0
Normalized_Temp_Normalized_Rs_rank <- 2800

# Variogram fitting for normalized temperature
N_lags <- 35          # Number of lags
lag_value <- 100      # Lag increment
Normalized_Temp_aclhs_sub50_vario_model<- 2
Normalized_Temp_aclhs_sub50_nugget<- 0.01
Normalized_Temp_aclhs_sub50_sill_and_nugget<- 1.4
Normalized_Temp_aclhs_sub50_rank <- 2800
# Fit variogram model visually
Normalized_Temp_aclhs_sub50_EyeModelVarioFit<-EyeModel(X_aclhs_sub50, Y_aclhs_sub50, 
                                           Normalized_Temp_aclhs_sub50$nscore, 0, 90, N_lags, lag_value, 1, 
                                           Normalized_Temp_aclhs_sub50_vario_model, Normalized_Temp_aclhs_sub50_nugget, Normalized_Temp_aclhs_sub50_sill_and_nugget, Normalized_Temp_aclhs_sub50_rank,
                                           "")

# Variogram fitting for normalized CO2
N_lags <- 35
lag_value <- 100
Normalized_Rs_aclhs_sub50_vario_model<- 2
Normalized_Rs_aclhs_sub50_nugget<- 0.1
Normalized_Rs_aclhs_sub50_sill_and_nugget<- 1.4
Normalized_Rs_aclhs_sub50_rank <- 2800
# Fit variogram model visually
Normalized_Rs_aclhs_sub50_EyeModelVarioFit<-EyeModel(X_aclhs_sub50, Y_aclhs_sub50, 
                                                       Normalized_Rs_aclhs_sub50$nscore, 0, 90, N_lags, lag_value, 1, 
                                                       Normalized_Rs_aclhs_sub50_vario_model, Normalized_Rs_aclhs_sub50_nugget, Normalized_Rs_aclhs_sub50_sill_and_nugget, Normalized_Rs_aclhs_sub50_rank,
                                                       "")
# Cross-variogram fitting
ModelVariogram(X_aclhs_sub50, Y_aclhs_sub50, 
               Normalized_Temp_aclhs_sub50$nscore, Normalized_Rs_aclhs_sub50$nscore, 
               N_lags, lag_value, 0, 90, 
               Normalized_Temp_Normalized_Rs_vario_model, 
               Normalized_Temp_sill_minus_nugget, Normalized_Rs_sill_minus_nugget, Normalized_Temp_Normalized_Rs_sill_minus_nugget, 
               Normalized_Temp_nugget, Normalized_Rs_nugget, Normalized_Temp_Normalized_Rs_nugget, Normalized_Temp_Normalized_Rs_rank, 
               'Normalized_Temp Variogram', 'Normalized_Rs Variogram', 'Normalized_Temp-Normalized_Rs CrossVariogram')

# Ensure positive definiteness of variogram model
NugetMatrix <- matrix(c(Normalized_Rs_nugget,
                        Normalized_Temp_Normalized_Rs_nugget,
                        Normalized_Temp_Normalized_Rs_nugget,
                        Normalized_Temp_nugget), ncol = 2)
det(NugetMatrix)
SemiVariMatrix1 <- c(Normalized_Rs_sill_minus_nugget,
                     Normalized_Temp_Normalized_Rs_sill_minus_nugget,
                     Normalized_Temp_Normalized_Rs_sill_minus_nugget,
                     Normalized_Temp_sill_minus_nugget)
SillMatrix <- matrix(SemiVariMatrix1, ncol = 2)
Sill_NuggetMatrix <- SillMatrix - NugetMatrix
which(Sill_NuggetMatrix < 0) 
det(SillMatrix)
if (any(Sill_NuggetMatrix < 0)) {
  stop("Sill-Nugget matrix contains negative values.")
}
det(SillMatrix)

# Data frame with coordinates (x, y) and two variables (var1, var2)
data <- data.frame(
  x = X_aclhs_sub50,                 
  y = Y_aclhs_sub50,  
  var1 = Normalized_Temp_aclhs_sub50$nscore,   
  var2 = Normalized_Rs_aclhs_sub50$nscore     
)
coordinates(data) <- ~x + y

# Variogram model for var1
vgm_var1 <- vgm(psill = 1.4, model = "Sph", range = 2800, nugget = 0.01)

# Variogram model for var2
vgm_var2 <- vgm(psill = 1.4, model = "Sph", range = 2800, nugget = 0.1)

# Cross-variogram model
vgm_cross <- vgm(psill = 0.45, model = "Sph", range = 2800, nugget = 0)

# Create the gstat object and add variogram models
g <- gstat(NULL, id = "var1", formula = var1 ~ 1, data = data, model = vgm_var1)
g <- gstat(g, id = "var2", formula = var2 ~ 1, data = data, model = vgm_var2)
g <- gstat(g, id = c("var1", "var2"), model = vgm_cross)

# Conditional co-simulation
Normalized_Temp <- nscore(Temp)
grid <-  data.frame(
  x = X,       
  y = Y
  ,var1 = Normalized_Temp$nscore
)
coordinates(grid) <- ~x + y

# Perform conditional cosimulation
system.time(Cond_SGCoSim <- predict(g, newdata = grid, nsim = 10))
# user  system elapsed 
# 290.574 194.138  74.958 nsim = 10
# Convert the results to a data frame
Cond_SGCoSim_df <- as.data.frame(Cond_SGCoSim)

# Back-transform
sim_backTrans_Rs_nsim = NULL
for (i in 13:22) {
  sim_back = backtr(Cond_SGCoSim_df[,i], Normalized_Rs_aclhs_sub50, tails='none', draw=TRUE)
  sim_backTrans_Rs_nsim  = cbind(sim_backTrans_Rs_nsim, sim_back)
}

# Verify the conditional or training points
cbind(Rs_aclhs_sub50,sim_backTrans_Rs_nsim[aclhs_sub50,])
sim_backTrans_Rs_nsim[aclhs_sub50,] <- Rs_aclhs_sub50

# UQ (uncertainty quantification)
UQ_min <- apply(sim_backTrans_Rs_nsim[,], 1, min)
UQ_max <- apply(sim_backTrans_Rs_nsim[,], 1, max)
UQ_max_min <- abs(UQ_max-Rs) + abs(Rs-UQ_min)
sum(UQ_max_min)

#------------------------------------------------------------------------------#
# 2. Implementation of the Copula-based Co-Simulation (CopCoSim) Method        #
#------------------------------------------------------------------------------#
#---------------Simulations of Rs conditioned to Temp--------------------------#
# Prepare training data for modeling and simulating using Bernstein Copula
aclhs_Sample <- data.frame(Temp_aclhs_sub50, Rs_aclhs_sub50) 
muestra <- aclhs_Sample
matriz.copem <- genmat.copem(muestra)

# Number of simulations per Temp point
m <- 10 

# Ensure Temp values are within the range of observed or training data
Temp_aux = Temp
for (i in 1:length(Temp_aux)) {
  if (Temp_aux[i] > max(Temp_aclhs_sub50) ){
    Temp_aux[i] = max(Temp_aclhs_sub50)
  }
  if (Temp_aux[i] < min(Temp_aclhs_sub50) ){
    Temp_aux[i] = min(Temp_aclhs_sub50)
  }
}
system.time(sim.m_aclhs <- mapply(simula.Bernshtein.condicional, Temp_aux, rep(m, length(Temp_aux))))
# sim.m_aclhs is a matrix with m rows and length(Temp) columns
# be patient... 63 seconds approx

sim.m_aclhs_UQ_min <- apply(sim.m_aclhs, 2, min)
sim.m_aclhs_UQ_max <- apply(sim.m_aclhs, 2, max)
sim.m_aclhs_UQ <- abs(sim.m_aclhs_UQ_max-Rs) + abs(Rs-sim.m_aclhs_UQ_min)

#---------------------Optimization Setup---------------------------------------#
Rs_aclhs_nsim <- NULL
for (i in 1:10) {
  #-----------Optimization------------------#
  n_sim <- length(Rs)
  Fn_limit <- seq(0,1,length.out = n_sub+1)
  Rs_aclhs_sub50_limit <- quantile(Rs_aclhs_sub50,Fn_limit)
  
  #--------------OF1------------#
  Func01sim  <- function(var_sim) {
    hist_Rs_sim <- hist(var_sim, breaks = Rs_aclhs_sub50_limit, plot = FALSE)$counts
    freq <- rep(n_sim/n_sub,n_sub)
    return (sum(abs(hist_Rs_sim-freq)))
  }
  
  
  corP_Temp_Rs_aclhs_sub50 <- round(cor(Temp_aclhs_sub50,Rs_aclhs_sub50, method = "pearson"),3)
  corS_Temp_Rs_aclhs_sub50 <- round(cor(Temp_aclhs_sub50,Rs_aclhs_sub50, method = "spearman"),3)
  corK_Temp_Rs_aclhs_sub50 <- round(cor(Temp_aclhs_sub50,Rs_aclhs_sub50, method = "kendall"),3)
  #------------OF2----------------#
  Func02sim  <- function(var_sim) {
    corP_Temp_var_sim <- round(cor(Temp,var_sim, method = "pearson"),3)
    corS_Temp_var_sim <- round(cor(Temp,var_sim, method = "spearman"),3)
    corK_Temp_var_sim <- round(cor(Temp,var_sim, method = "kendall"),3)
    return (abs(corP_Temp_var_sim-corP_Temp_Rs_aclhs_sub50) + abs(corS_Temp_var_sim-corS_Temp_Rs_aclhs_sub50) + abs(corK_Temp_var_sim-corK_Temp_Rs_aclhs_sub50)) 
  }
  
  
  #Variogram models (1- exponential, 2- spherical, 3- gaussian)
  N_lags<- 35
  lag_value <- lag_value
  Rs_vario_model<- 2 
  Rs_nugget<- 13000 
  Rs_sill_and_nugget<- 125000
  Rs_range <- 3000
  Modelo1 = Rs_vario_model
  if (Modelo1 == 1) {
    ModeloA <- "exponential"
  }
  if (Modelo1 == 2) {
    ModeloA <- "spherical"
  }
  if (Modelo1 == 3) {
    ModeloA <- "gaussian"
  }
  Sill = Rs_sill_and_nugget - Rs_nugget
  Range = Rs_range
  VMod = (Rs_sill_and_nugget) - cov.spatial(seq(lag_value,N_lags*lag_value,lag_value), cov.model = ModeloA, 
                                            cov.pars = c(Sill, Range))
  semi_prior = VMod
  #------------OF3------------#
  Func03sim <- function(var_sim) {
    N_lags<- 35
    lag_value <- 100
    X_sim <- X
    Y_sim <- Y
    Vario_var_sim <- variog(as.geodata(cbind(X_sim, Y_sim, var_sim), 
                                       coords.col = 1:2, data.col = 3), breaks = c(seq(lag_value/2, lag_value * (N_lags+1), lag_value)),
                            trend = "cte", lambda = 1, estimator.type = "classical", nugget.tolerance = 0, direction = 0, 
                            tolerance = 90, unit.angle = "degrees", pairs.min = 1)
    return (sum(seq(10,1,length.out= N_lags)*abs(Vario_var_sim$v-semi_prior))) # Rs_VarioEstimation[,3] seq(100,1, by = 5, length.out = N_lags)
  }
  #Combined Objective Function
  ws1 = 1; ws2 = 100; ws3 = 0.0001 
  FuncConjsim  <- function(var_sim) {
    total =  ws1*Func01sim(var_sim)  + ws2*Func02sim(var_sim) + ws3*Func03sim(var_sim) 

    return (total) 
  }
  
  #DE
  Pop_Inicial_sim <- as.matrix(sim.m_aclhs)
  for (i in 1:length(aclhs_sub50)) {
    for (j in 1:nrow(Pop_Inicial_sim)) {
      Pop_Inicial_sim[j,aclhs_sub50[i]] = Rs_aclhs_sub50[i]
    }
  }
  vector_min =  apply(sim.m_aclhs, 2, min) 
  vector_max =  apply(sim.m_aclhs, 2, max) 
  vector_min[aclhs_sub50] = Rs_aclhs_sub50
  vector_max[aclhs_sub50] = Rs_aclhs_sub50
  system.time(
    aclhs_sim_outDEoptim_Rs1 <- DEoptim(fn=FuncConjsim, lower= vector_min,
                                        upper= vector_max,
                                        control = DEoptim.control(VTR = 0.0000001,strategy = 3, 
                                                                  itermax = 10000, reltol = 1e-8,  # 40000
                                                                  CR = 0.5, F = 0.8,NP= nrow(Pop_Inicial_sim), initialpop = Pop_Inicial_sim)) # 
  )

  aclhs_sim_outDEoptim_Rs$optim$bestval
  Ite <- seq(1,length(aclhs_sim_outDEoptim_Rs$member$bestvalit),1)
  plot(Ite,aclhs_sim_outDEoptim_Rs$member$bestvalit, "l", xlab ="Iterations", ylab ="Objective Function")
  var_sim <- as.numeric(aclhs_sim_outDEoptim_Rs$optim$bestmem)
  
  #------------------- tables-------------------#
  # Statistical Properties
  Rs_aclhs_pred1 <- aclhs_sim_outDEoptim_Rs$optim$bestmem
  Rs_aclhs_pred1[aclhs_sub50] <- Rs_aclhs_sub50
  
  Rs_aclhs_nsim <- cbind(Rs_aclhs_nsim,Rs_aclhs_pred1)
}

#----------------------Uncertainty Quantification-------------------------------#
Rs_aclhs_nsim[aclhs_sub50,]
Rs_aclhs_nsim_UQ_min <- apply(Rs_aclhs_nsim, 1, min)
Rs_aclhs_nsim_UQ_max <- apply(Rs_aclhs_nsim, 1, max)
Rs_aclhs_nsim_UQ <- abs(Rs_aclhs_nsim_UQ_max-Rs) + abs(Rs-Rs_aclhs_nsim_UQ_min)
sum(Rs_aclhs_nsim_UQ)
sum(UQ_max_min)/sum(Rs_aclhs_nsim_UQ)

