

agreement_Stats_Sim <- function(data){
  
  source("scripts_and_functions/MSD_Sim.R")
  source("scripts_and_functions/CCC_Sim.R")
  source("scripts_and_functions/accuracy_Sim.R")
  source("scripts_and_functions/precision_Sim.R")
  
  list(c( MSD=MSD_Sim(data), CCC=CCC_Sim(data), accuracy=accuracy_Sim(data), 
          precision=precision_Sim(data)))
  
}
