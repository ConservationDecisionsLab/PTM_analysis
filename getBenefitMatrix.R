#' ---
#' title: "Generate Benefits Matrix for Complementarity Analysis"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "3 July 2019"
#' output: github_document
#' ---
#'
#' This code weights benefits by feasibility, recalculates expected performance based on weighted benefit estimates, 
#' and generates the benefit matrix for use in the complementarity analysis. Based on 1_Cost-Effectiveness.R code 
#' from FRE PTM project, but using a different (shorter) way to implement
#' 
#' Requires **Aggregated_Benefits.csv** and **Aggregated_Baseline.csv** from *aggregateEstimates.R*, and a
#' **CostFeas** table of strategy costs and feasibilities
#'
#+ warning = FALSE, message = FALSE
library(tidyverse)

#' Use results from *aggregateEstimates.R*
#+ warning = FALSE, message = FALSE
ben.mat.agg <- read_csv("Aggregated_Benefits_Newcombos.csv")
base.mat.agg <- read_csv("Aggregated_Baseline_rev.csv")

#' Create table of Cost and Feasibility if needed for TESTING purposes
# feas <- c(1, runif(22, min = 0.5, max = 0.95))
# cost <- c(0, runif(22, min = 1000000, max = 400000000))
# Strategy <- c("Baseline", paste0("S", seq(1:22)))
# costfeas <- as.data.frame(cbind(cost, feas))
# names(costfeas)<- c("Cost", "Feasibility")
# costfeas <- cbind(Strategy, costfeas)
# write_csv(costfeas, "sample_CostFeas.csv")

#' Read in Cost & Feasibility table
#+ warning = FALSE, message = FALSE
costfeas <- read_csv("CostFeas2_Newcombos.csv") # sample file created by above code

print(costfeas)

costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as.character(costfeas$Strategy)
costfeas$Strategy <- as_factor(costfeas$Strategy)

#' Calculate the expected benefits (weighted by feasibility)
# Tidy data
rlong.ben <- gather(ben.mat.agg, key = Estimate, value = Value, 
                    colnames(ben.mat.agg)[2]:colnames(ben.mat.agg)[ncol(ben.mat.agg)]) %>%
  separate(., Estimate, c("Est.Type", "Strategy"), sep = "[_]", remove = FALSE)
rlong.ben$Strategy <- as_factor(paste0("S", rlong.ben$Strategy))

# Join with cost & feasibility table then weight benefits by feasibility
joined.data <- left_join(rlong.ben, costfeas, by = "Strategy") %>%
  mutate(., Wt.Value = Value * Feasibility)
names(joined.data)[1] <- "Ecological.Group"

# Spread table and output results
joined.wide <- joined.data %>%
  select(., c(Ecological.Group, Estimate, Wt.Value)) %>%
  spread(key = Estimate, value = Wt.Value)
est.levels <- unique(joined.data$Estimate)
joined.wide <- joined.wide[, c("Ecological.Group", est.levels)] # rearranges columns so strategies are in the correct order

write_csv(joined.wide, "Aggregated_Benefits_weighted.csv")

#' Calculate expected performance based on weighted benefit estimates
# Join with baseline estimates to make sure the observations (Ecol. groups) line up correctly
# then split again to add weighted benefits to (averaged) baseline and get the expected performance
joinedbase.wide <- left_join(base.mat.agg, joined.wide, by = "Ecological.Group") 

base.mat <- joinedbase.wide[,2:4]
perf.mat <- joinedbase.wide[,5:ncol(joinedbase.wide)] + as.matrix(base.mat)

perf.mat <- cbind(joinedbase.wide$Ecological.Group,base.mat,perf.mat)
names(perf.mat)[1] <- "Ecological.Group"

write_csv(perf.mat, "Aggregated_Performance_weighted.csv")

#' Generate (weighted) benefits matrix for optimization
wt.ben <- perf.mat %>%
  select(., c(Ecological.Group, contains("Best.guess"))) # select only Best Guess estimates from perf.mat
wt.ben.t <- data.frame(t(wt.ben[,-1]))
names(wt.ben.t) <- wt.ben$Ecological.Group # column names

# Also get upper and lower values for uncertainty analysis
wt.ben.low <- perf.mat %>%
  select(., c(Ecological.Group, contains("Lower"))) # select only Best Guess estimates from perf.mat
wt.ben.low.t <- data.frame(t(wt.ben.low[,-1]))
names(wt.ben.low.t) <- wt.ben.low$Ecological.Group # column names

wt.ben.hi <- perf.mat %>%
  select(., c(Ecological.Group, contains("Upper"))) # select only Best Guess estimates from perf.mat
wt.ben.hi.t <- data.frame(t(wt.ben.hi[,-1]))
names(wt.ben.hi.t) <- wt.ben.hi$Ecological.Group # column names


# Create vector of strategy names to add to the table
strat.names <- vector()
strat.names[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rownames(wt.ben.t)[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
strat.names[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # Rows without the "_" are Baseline estimates

wt.ben.t <- cbind(strat.names,wt.ben.t)
names(wt.ben.t)[1] <- "Strategy"

# print(wt.ben.t, row.names = FALSE)

wt.ben.low.t <- cbind(strat.names, wt.ben.low.t)
names(wt.ben.low.t)[1] <- "Strategy"

wt.ben.hi.t <- cbind(strat.names, wt.ben.hi.t)
names(wt.ben.hi.t)[1] <- "Strategy"

write_csv(wt.ben.t, "Benefits.csv") # use this table for the complementarity analysis
write_csv(wt.ben.low.t, "Lower.csv")
write_csv(wt.ben.hi.t, "Upper.csv")
