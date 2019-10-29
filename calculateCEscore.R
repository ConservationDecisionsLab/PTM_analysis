#' ---
#' title: "Calculate Cost-effectiveness"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "3 July 2019"
#' output: github_document
#' ---

#' This code calculates cost-effectiveness scores and ranks strategies by Benefit, Cost, and CE
#' Based on algorithm from Step 2 section of *1_Cost-Effectiveness.R* code from FRE PTM project,
#' but using a different way to implement.  
#' 
#' Also performs uncertainty analysis for costs.  
#' 
#' Requires **Aggregated_Benefits.csv** from *aggregateEstimates.R*, and a **CostFeas.csv** table of strategy cost and feasibility.  

#' Load packages
#+ warning = FALSE, message = FALSE
library(tidyverse)
library(mc2d)
library(cowplot)

#' Set parameters
a <- 1000000 # scaling to get cost and benefits in same order of magnitude
uncrtn.anal <- 0 # 1 if running cost uncertainty analysis, 0 if not.
MC <-  10000 # number of iterations
u <- 0.3 # prop. variation from the best estimate

#' Use result from *aggregateEstimates.R*
#+ warning = FALSE, message = FALSE
ben.mat.agg <- read_csv("Aggregated_Benefits_groupWtd_Newcombos.csv")

#' Read in Cost & Feasibility table
#+ warning = FALSE, message = FALSE
costfeas <- read_csv("CostFeas2_revised.csv")
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as.character(costfeas$Strategy)
costfeas$Strategy <- as_factor(costfeas$Strategy)

#' Calculate cost-effectiveness score: CE = (Ben*Feas)/Cost
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

if (uncrtn.anal == 0) {
  #' Join with cost/feasibility table and calculate cost-effectiveness
  strat.est <- full_join(sum.ben, costfeas, by="Strategy") %>%
    mutate(., Sc.Cost = Cost/a, # scale costs to get reasonable values
           Exp.Benefit = Benefit * Feasibility, # weight benefits
           CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores
  
  #' Rank strategies by (weighted)Benefit, Cost, and CE score
  CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
    mutate(., CE_rank = rank(-CE), 
           ExpBenefit_rank = rank(-Exp.Benefit), 
           Cost_rank = rank(Cost))
  print(CE_Score)
  
  #' Output results
  write_csv(CE_Score, "Cost_Effectiveness_grpwtd2_revised.csv")

} else {
  
  #' Do uncertainty analysis
  samples <- matrix(nrow = nrow(costfeas),ncol = MC)
  MC.CE_Score <- list()
  # get min and max
  min.Cost <- costfeas$Cost * (1-u)
  max.Cost <- costfeas$Cost * (1+u)
  best.Cost <- costfeas$Cost
  for (it in 1:MC) {
    rnd <- runif(1,1,999999)
    set.seed(rnd)
    # sample cost values
    samples[,it] <- rpert(nrow(costfeas),
                          min = min.Cost,
                          mode =best.Cost,
                          max = max.Cost, shape=4)
    costfeas$Cost <- samples[,it]

    #' Join with cost/feasibility table and calculate cost-effectiveness
    strat.est <- full_join(sum.ben, costfeas, by="Strategy") %>%
      mutate(., Sc.Cost = Cost/a, # scale costs to get reasonable values
             Exp.Benefit = Benefit * Feasibility, # weight benefits
             CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores

    #' Rank strategies by (weighted)Benefit, Cost, and CE score
    CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
      mutate(., CE_rank = rank(-CE),
             ExpBenefit_rank = rank(-Exp.Benefit),
             Cost_rank = rank(Cost))
    MC.CE_Score[[it]] <- CE_Score
  }
  MC.CE_Table <- lapply(MC.CE_Score, "[", 1:length(strat.names), "CE")
  MC.Results <- matrix(unlist(MC.CE_Table), ncol = MC, byrow = FALSE)
  MC.Results <- data.frame(costfeas$Strategy, MC.Results)
  names(MC.Results)[1] <- "Strategy"

  #' Save results
  write_csv(MC.Results, "MC_CEScores2_cost.csv")
  MC_Samples <- data.frame(costfeas$Strategy, samples)
  write_csv(MC_Samples, "MC_PerfSamples2_cost.csv")

  #' Box plots for visualization
  MC.CE <- gather(MC.Results, key = MC.Iter, value = CE, 2:ncol(MC.Results))
  MC.CE$Strategy <- as_factor(MC.CE$Strategy)

  temp.plot <-
    ggplot(MC.CE, aes(x = Strategy, # for each Ecological group, plot Estimate Type on x-axis
                      y = CE # and St.Value on y-axis,
    ) # and colour the boxplots by estimate type
    ) +
    geom_boxplot() + # tell R to display data in boxplot form
    theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
    theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # adjust margins around the outside of the plot (top, right, bottom, left)
          panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
          axis.title.y = element_text(margin = margin(t = 0,
                                                      r = 10,
                                                      b = 0,
                                                      l = 0) # adjust space between y-axis numbers and y-axis label
          ),
          plot.caption = element_text(size = 10, hjust = 0)
    ) +
    # facet_wrap( ~ Strategy, nrow = 3) +  # create a separate panel of estimates for each management strategy
    scale_x_discrete(name = "Management strategies",
                     breaks = MC.CE$Strategy,
                     labels = MC.CE$Strategy# Give the x-axis variables labels
    ) +
    # scale_fill_manual(values = c("white", "gray80", "white"), # Assign colours to each type of estimate and don't show a legend
    #                   guide = FALSE
    # ) +
    labs(x = "Management Strategies",
         y = "Cost-effectiveness score"
         # title = paste(grp.levels[i]),
         # caption = paste0(
         # "Figure ", i, ". Boxplots summarizing the distribution of the lower (L), best guess (B), and upper (Upper) expert estimates of the probability of persistence
         # of ", grp.levels[i], " under the Baseline scenario and each of the management strategies (S1 - S22). Lower and Upper estimates
         # have been standardized to 80% confidence level. The thick horizontal lines indicate the median estimate, while the surrounding box shows the
         # interquartile range. Any outliers are shown as points beyond the plot whiskers.Your individual estimates, standardized to 80% confidence level,
         # are shown in blue.")
    ) +
    ylim(0, 200) # set the y-axis limits from 0-100

}

