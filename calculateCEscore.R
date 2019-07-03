#' ---
#' title: "Calculate Cost-effectiveness"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "3 July 2019"
#' output: github_document
#' ---
 
#' This code calculates cost-effectiveness (CE) scores and ranks strategies by Benefit, Cost, and CE
#' Based on algorithm from Step 2 section of 1_Cost-Effectiveness.R code from FRE PTM project,
#' but using a different way to implement


library(tidyverse)

#' Use result from aggregateEstimates.R
ben.mat.agg <- read_csv("Aggregated_Benefits.csv")

#' Create table of Cost and Feasibility FOR TESTING ONLY 
# feas <- c(1, runif(22, min = 0.5, max = 0.95))
# cost <- c(0, runif(22, min = 1000000, max = 400000000))
# Strategy <- c("Baseline", paste0("S", seq(1:22)))
# costfeas <- as.data.frame(cbind(cost, feas))
# names(costfeas)<- c("Cost", "Feasibility")
# costfeas <- cbind(Strategy, costfeas)
# write_csv(costfeas, "sample_CostFeas.csv")

#' Read in Cost & Feasibility table
costfeas <- read_csv("sample_CostFeas.csv") # sample file created by above code
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as.character(costfeas$Strategy)
costfeas$Strategy <- as_factor(costfeas$Strategy)

#' Calculate cost-effectiveness score: 
#' CE = (Ben*Feas)/Cost
# Get Best Guess estimates and transpose so that Strategies are in rows and Groups are in columns
strat.ben <- data.frame(t(select(ben.mat.agg, contains("Best.guess"))))
names(strat.ben) <- ben.mat.agg$Ecological.Group

# Create vector of strategy names
strat.names <- vector()
strat.names[which(str_detect(rownames(strat.ben), "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rownames(strat.ben)[which(str_detect(rownames(strat.ben), "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
strat.names[which(str_detect(rownames(strat.ben), "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # Rows without the "_" are Baseline estimates

# Add up (unweighted) aggregated benefits of each strategy across ecological groups
sum.ben <- data.frame(strat.names, rowSums(strat.ben))
names(sum.ben) <- c("Strategy", "Benefit")
sum.ben$Strategy <- as_factor(as.character(sum.ben$Strategy))

# Join with cost/feasibility table and calculate cost-effectiveness
strat.est <- full_join(sum.ben, costfeas, by="Strategy") %>%
  mutate(., Sc.Cost = Cost/1000000, # scale costs to get reasonable values
         Exp.Benefit = Benefit * Feasibility, # weight benefits
         CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores

#' Rank strategies by (weighted)Benefit, Cost, and CE score
CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
  mutate(., CE_rank = rank(-CE), 
         ExpBenefit_rank = rank(-Exp.Benefit), 
         Cost_rank = rank(Cost))

#' Output results
write_csv(CE_Score, "Cost_Effectiveness.csv")
