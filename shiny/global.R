# Load relevant libraries
library(shiny)
library(shinyBS)
library(shinyjs)
library(marr)
library(MSPrep)
library(DT)
library(ggplot2)
library(dplyr)
library(rlang)
library(markdown)

# Prepare example data using MSPrep
data(msquant)

summarizedDF <- msSummarize(msquant,
                            compVars = c("mz", "rt"),
                            sampleVars = c("spike", "batch", "replicate", 
                                           "subject_id"),
                            cvMax = 0.50,
                            minPropPresent = 1/3,
                            returnSummaryDetails = FALSE,
                            colExtraText = "Neutral_Operator_Dif_Pos_",
                            separator = "_",
                            missingValue = 1,
                            returnToSE = FALSE)

filteredDF <- msFilter(summarizedDF,
                       filterPercent = 0.8,
                       compVars = c("mz", "rt"),
                       sampleVars = c("spike", "batch", "subject_id"),
                       separator = "_",
                       returnToSE = FALSE)

bpcaImputedDF <- msImpute(filteredDF, imputeMethod = "bpca",
                          compVars = c("mz", "rt"),
                          sampleVars = c("spike", "batch", "subject_id"),
                          separator = "_",
                          returnToSE = FALSE,
                          missingValue = 0)

msprepData <- bpcaImputedDF %>%
    mutate_all(as.numeric)
msprepDataNoFeats <- bpcaImputedDF[, 3:20]
colnames(msprepData)[3:20] <- paste0("X", colnames(msprepData))[3:20]
colnames(msprepDataNoFeats) <- paste0("X", colnames(msprepDataNoFeats))