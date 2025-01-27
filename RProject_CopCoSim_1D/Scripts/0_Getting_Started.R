#--------------------------------------------------------------#
#                     Getting Started Script                   #
#--------------------------------------------------------------#

#### Packages Installation ####
# Define the root working directory
root_dir <- getwd()

# Install necessary packages
install.packages("geoR")
install.packages("moments")
install.packages("sp")
install.packages("clhs")
install.packages("DEoptim")
install.packages("gstat")

# Set back to root working directory
setwd(root_dir)

#### Load Packages ####
library(geoR)
library(moments)
library(sp)
library(DEoptim)
library(clhs)
library(gstat)

#### Load Workspace ####
# Uncomment and update the file path if you need to load a workspace
# load("path_to_workspace.RData")

#### Load Functions ####
# Define the functions directory
function_dir <- file.path(root_dir, "/Functions")
setwd(function_dir)

# Source all required function files
source_files <- c(
  "AllModel.R", "BasicStats.R", "BestModel.R", "BestModel_mod.R", 
  "BestModelName.R", "CDF.R", "SGS.R", "CoKrigingOrd.R", 
  "CoKrigingOrd_mod.R", "CoKrigingOrdAnis.R", "CoKrigingOrdAnis_mod.R", 
  "CrossValidation.R", "CrossValidation2.R", "CrossVariograma.R", 
  "CrossVariogram_exp.R", "DEspacial.R", "Distance.R", "Estadisticas.R", 
  "EyeModel.R", "FitDistribution.R", "GDEspacial.R", "GDirecciones.R", 
  "GNormal.R", "hist2.R", "HistBoxplot.R", "HistModel.R", "KrigingOrd.R", 
  "KrigingOrd_mod.R", "KrigingOrdAnis.R", "KrigingOrdAnis_mod.R", 
  "ModelVariogram.R", "Modelo.R", "Outliers.R", "OutliersCount.R", 
  "OutliersCountTwo.R", "OutliersPos.R", "OutliersTwo.R", "PPplot.R", 
  "QQplot.R", "RangoParams.R", "Regresion.R", "ScatterPlot.R", 
  "Tendencia.R", "Transformacion.R", "Trend.R", "Val_Estadisticos.R", 
  "Validacion.R", "ValidacionCross.R", "Variograma.R", "Variograma4D.R", 
  "scaterplot.R", "scaterplotReg.R", "nscore.R", 
  "Bernstein.R", "Bernstein1.R"
)

# Source each file
for (file in source_files) {
  source(file, encoding = "ISO-8859-1")
}

# Set back to root working directory
setwd(root_dir)