# CopCoSim_Le_Vargas
Code to reproduce workflow in Le and Vargas(in review)


# Copula-based Cosimulation (CopCoSim)

by Van Huong Le<sup>1</sup>, Rodrigo Vargas<sup>1</sup>

<sup>1</sup>Department of Plant and Soil Sciences, University of Delaware, Newark, DE, 19716, USA

Corresponding author affiliation and e-mail:

Rodrigo Vargas

Department of Plant and Soil Sciences, University of Delaware, Newark, DE, 19716, USA

[rvargas\@udel.edu](mailto:rvargas@udel.edu)

## Description

This repository contains the source code to perform different simulation methods: the traditional Sequential Gaussian CoSimulation (SGCoSim) and copula-based cosimulation (CopCoSim). Here are two case studies using data of soil CO$_{2}$ efflux (i.e., the CO$_{2}$  efflux from soils to the atmosphere known as soil respiration) that are relevant for carbon cycle science.
## Content

##### RProject_CopCoSim_1D: The case study represents data from a time series

-   Data: this folder contains the data
-   Functions: this folder contains the useful functions
-   Scripts: this folder contains the scripts
-   Results: this folder contains the results
-   RProject_Simulations_1D.Rproj: This file is the R Project

##### RProject_CopCoSim_2D: The case study represents data from spatial data

-   Data: this folder contains the data
-   Functions: this folder contains the useful functions
-   Scripts: this folder contains the scripts
-   Results: this folder contains the results
-   RProject_Simulations_2D.Rproj: This file is the R Project

## install

The code has been tested using packages of:

-   R version 4.3.1

-   RStudio 2023.06.1

## How to run the code?

#### RProject_CopCoSim_1D

Opening the project `RProject_Simulations_1D.Rproj` with Rstudio. Then open all the scripts in the "scripts" folder. The scripts are run in the following order: 0_Getting_Started.R, 1_Selection_of_a_representative_training_dataset.R, 2_Application_of_stochastic_simulation_methods.R, 3_Evaluation_of_model_performance.R.

-   0_Getting_Started.R: This script installs and loads R packages, as well as loads functions from the functions folder.
-   1_Selection_of_a_representative_training_dataset.R: This script explores and calculates the univariate statistical properties and dependency relationships between variables, along with the temporal or spatial distribution of the variable of interest. It also calculates its autocorrelation function and applies the sampling method (i.e., acLHS) based on the data.
-   2_Application_of_stochastic_simulation_methods.R: This script models the characteristic functions of the variables using the SGCoSim and CopCoSim methods and performs the simulations.
-   3_Evaluation_of_model_performance.R: This script produces the final figures and evaluates the model performance.


#### RProject_CopCoSim_2D

Opening the project `RProject_Simulations_2D.Rproj` with Rstudio. Then open all the scripts in the "scripts" folder. The scripts are run in the following order: 0_Getting_Started.R, 1_Selection_of_a_representative_training_dataset.R, 2_Application_of_stochastic_simulation_methods.R, 3_Evaluation_of_model_performance.R.

-   0_Getting_Started.R: This script installs and loads R packages, as well as loads functions from the functions folder.
-   1_Selection_of_a_representative_training_dataset.R: This script explores and calculates the univariate statistical properties and dependency relationships between variables, along with the temporal or spatial distribution of the variable of interest. It also calculates its autocorrelation function and applies the sampling method (i.e., acLHS) based on the data.
-   2_Application_of_stochastic_simulation_methods.R: This script models the characteristic functions of the variables using the SGCoSim and CopCoSim methods and performs the simulations.
-   3_Evaluation_of_model_performance.R: This script produces the final figures and evaluates the model performance.


## License

Source code to replicate figures in publication:
"Copula-based Cosimulation for Simulating Temporal or Spatial Data in Biogeosciences "  

By Van Huong Le and Rodrigo Vargas (in review)
