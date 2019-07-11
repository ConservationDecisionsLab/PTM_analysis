#' ---
#' title: "Aggregate Standardized Benefit Estimates"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "3 July 2019"
#' output: github_document
#' ---
#' 
#' This code  
#' a) calculates benefits of each strategy (strategy performance - baseline performance) for each ecological group,  
#' b) aggregates (averages) across experts, and  
#' c) calculates expected performance under each strategy based on the aggregated benefit estimate.  
#' Based on first part of Step 2 section of 1_Cost-Effectiveness.R code from FRE PTM project and
#' uses **Standardized_Estimates_Wide.csv** from *standardizeConfidence.R* 
#' 
#+ warning = FALSE, message = FALSE
library(tidyverse)

#' Read in data
#+ warning = FALSE, message = FALSE
rlong.wide <- read_csv("Standardized_Estimates_Wide.csv")
rlong.wide$Expert <- as_factor(rlong.wide$Expert)
rlong.wide$Ecological.Group <- as_factor(rlong.wide$Ecological.Group)

#' Remove confidence estimates from the table
wide.colnames <- colnames(rlong.wide)
idx.colnames <- which(str_detect(wide.colnames, "Confidence") == 1)
DF <- rlong.wide[,-(idx.colnames)]

#' Calculate benefit: subtract baseline performance from strategy performance for each expert
base.mat <- DF[3:5]
strat.mat <- DF[6:ncol(DF)]  

ben.mat <- strat.mat - as.matrix(base.mat)
ben.mat[ben.mat<0] <- 0

#' Aggregate benefit: average benefit estimates for each species group + strategy across experts
ben.mat <- cbind(DF[,1:2], ben.mat )
base.mat <- cbind(DF[,1:2], base.mat)

ben.mat.agg <- aggregate(ben.mat[,3:ncol(ben.mat)], by=list(ben.mat$Ecological.Group), FUN = mean, na.rm = TRUE) # should check this manually
base.mat.agg <- aggregate(base.mat[,3:ncol(base.mat)], by=list(base.mat$Ecological.Group), FUN = mean, na.rm = TRUE)

names(ben.mat.agg)[1] <- "Ecological.Group"
names(base.mat.agg)[1] <- "Ecological.Group"

write_csv(ben.mat.agg, "Aggregated_Benefits.csv")
write_csv(base.mat.agg, "Aggregated_Baseline.csv")

#' Calculate averaged performance: add averaged benefit estimates to (averaged) baseline
exp.pop <- ben.mat.agg[,2:ncol(ben.mat.agg)] + as.matrix(base.mat.agg[,2:ncol(base.mat.agg)])
exp.pop <- cbind(base.mat.agg, exp.pop)

#' Output results
write_csv(exp.pop, "Aggregated_Performance.csv")
