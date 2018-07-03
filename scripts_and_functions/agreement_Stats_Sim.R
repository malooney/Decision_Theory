

agreement_Stats_Sim <- function(data){
  
  source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/MSD_Sim.R")
  
  source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/CCC_Sim.R")
  
  source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/accuracy_Sim.R")
  
  source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/precision_Sim.R")
  
  list(c( MSD=MSD_Sim(data), CCC=CCC_Sim(data), accuracy=accuracy_Sim(data), 
          precision=precision_Sim(data)))
  
}
