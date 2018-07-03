

# Housekeeping ----------------------------------------------------------------
rm(list= ls())
cat("\014")

library(readr)
library(MASS)
library(MCMCpack)

source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/agreement_Stats_Sim.R")

source("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/scripts_and_functions/agreement_Stats.R")

#  ----------------------------------------------------------------------------
# Jeffrey's Prior Data

CCC_30_100_100_100_100_99_main_results <- read_csv("data/CCC_30_100_100_100_100_99_main_results.csv", col_types = cols(X1 = col_skip()))

CCC_100_100_100_100_100_99_main_results <- read_csv("data/CCC_100_100_100_100_100_99_main_results.csv", col_types = cols(X1 = col_skip()))


CCC_30_100_100_100_125_90_main_results <- read_csv("data/CCC_30_100_100_100_125_90_main_results.csv", col_types = cols(X1 = col_skip()))

CCC_100_100_100_100_125_90_main_results <- read_csv("data/CCC_100_100_100_100_125_90_main_results.csv", col_types = cols(X1 = col_skip()))

#  ----------------------------------------------------------------------------
# Independence Jeffrey's Prior Data

JIP_CCC_30_100_100_100_100_99_main_results <- read_csv("data/JIP_CCC_30_100_100_100_100_99_main_results.csv", col_types = cols(X1 = col_skip()))

JIP_CCC_100_100_100_100_100_99_main_results <- read_csv("data/JIP_CCC_100_100_100_100_100_99_main_results.csv", col_types = cols(X1 = col_skip()))


JIP_CCC_30_100_100_100_125_90_main_results <- read_csv("data/JIP_CCC_30_100_100_100_125_90_main_results.csv", col_types = cols(X1 = col_skip()))

JIP_CCC_100_100_100_100_125_90_main_results <- read_csv("data/JIP_CCC_100_100_100_100_125_90_main_results.csv", col_types = cols(X1 = col_skip()))

#  ----------------------------------------------------------------------------
# Reference Prior for rho Data

JRP_30_100_100_100_100_99_main_results <- read_csv("data/JRP_30_100_100_100_100_99_main_results.csv", col_types = cols(X1 = col_skip()))

JRP_100_100_100_100_100_99_main_results <- read_csv("data/JRP_100_100_100_100_100_99_main_results.csv", col_types = cols(X1 = col_skip()))


JRP_30_100_100_100_125_90_main_results <- read_csv("data/JRP_30_100_100_100_125_90_main_results.csv", col_types = cols(X1 = col_skip()))

JRP_100_100_100_125_100_90_main_results <- read_csv("data/JRP_100_100_100_100_125_90_main_results.csv", col_types = cols(X1 = col_skip()))

#  ----------------------------------------------------------------------------
# Jeffrey's Prior Simulation Results
agreement_Stats_Sim(CCC_30_100_100_100_100_99_main_results)
agreement_Stats_Sim(CCC_100_100_100_100_100_99_main_results)

agreement_Stats_Sim(CCC_30_100_100_100_125_90_main_results)
agreement_Stats_Sim(CCC_100_100_100_100_125_90_main_results)

#  ----------------------------------------------------------------------------
# Independence Jeffrey's Prior Simulation Results
agreement_Stats_Sim(JIP_CCC_30_100_100_100_100_99_main_results)
agreement_Stats_Sim(JIP_CCC_100_100_100_100_100_99_main_results)

agreement_Stats_Sim(JIP_CCC_30_100_100_100_125_90_main_results)
agreement_Stats_Sim(JIP_CCC_100_100_100_100_125_90_main_results)

#  ----------------------------------------------------------------------------
# Reference Prior for rho Simulation Results
agreement_Stats_Sim(JRP_30_100_100_100_100_99_main_results)
agreement_Stats_Sim(JRP_100_100_100_100_100_99_main_results)

agreement_Stats_Sim(JRP_30_100_100_100_125_90_main_results)
agreement_Stats_Sim(JRP_100_100_100_125_100_90_main_results)


# Kidney Data - Bayesian Inference ---------------------------------------------

IPIA_Data <- read.csv("/Users/malooney/Google Drive/digitalLibrary/*Decesion_Theory/Decision_Theory/data/IPIA_Data.csv")

JP_KidneyData_Bayes <- read_csv("data/JP_KidneyData_Bayes.csv", 
                                col_types = cols(X1 = col_skip()))

agreement_Stats(JP_KidneyData_Bayes)

par(mfrow= c(2, 2), ps= 12, cex= 0.55, cex.main= 1.5)

MSD(JP_KidneyData_Bayes, plot_MSD = 1)
CCC(JP_KidneyData_Bayes, plot_CCC = 1)
precision(JP_KidneyData_Bayes, plot_precision = 1)
accuracy(JP_KidneyData_Bayes, plot_accuracy = 1)












