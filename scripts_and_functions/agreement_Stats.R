

agreement_Stats <- function(data){
  
  source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/MSD.R")
  source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/CCC.R")
  source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/accuracy.R")
  source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/precision.R")

  list(c( MSD=MSD(data), CCC=CCC(data), accuracy=accuracy(data), 
          precision=precision(data)))
  
}
