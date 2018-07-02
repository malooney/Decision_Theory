

agreement_Stats <- function(data){
  
  source("scripts_and_functions/MSD.R")
  source("scripts_and_functions/CCC.R")
  source("scripts_and_functions/accuracy.R")
  source("scripts_and_functions/precision.R")

  list(c( MSD=MSD(data), CCC=CCC(data), accuracy=accuracy(data), 
          precision=precision(data)))
  
}
