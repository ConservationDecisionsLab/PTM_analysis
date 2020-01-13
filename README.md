# PTM_analysis

Collection of R scripts for standardizing and aggregating expert estimates and calculating cost-effectiveness. Includes sample Benefits worksheet table with the same structure as the raw Benefit scoring tables used in expert elicitation (contact abbey.camaclang@gmail.com for copies of actual benefits tables for code testing). Also included are: the cost/feasibility summary table; the list of species in each ecological grouping; and the list of special cases (where experts have provided benefit scores for only some of the species in an ecological group) from the Saint John River (SJR) PTM. Data tables provided for testing purposes only - please do not circulate or publish.  

### combineTables.R 
* reads individual expert estimate tables and combines them into single Results.csv file

### standardizeConfidence.R 
* takes Results.csv file and standardizes lower and upper estimates to 80% confidence level
* saves results into 2 tables: Standardized_Estimates_Long.csv (tidy version) and Standardized_Estimates_Wide.csv (same table format as Results.csv)
* also counts the number of expert estimates for each ecological group (Estimates_per_group.csv) and for each strategy-group combination (Estimates_by_strategy.csv), and saves a tidy version of Results.csv (Results_tidy.csv)

### createBoxplots.R 
* uses Standardized_Estimates_Long.csv to create plots for expert review & feedback
  + plot1 are boxplots of the best guess, lower, and upper estimates for each strategy, with separate plots for each ecological group
  + plot2 are pointrange plots of each individual expert estimate for each strategy, with separate plots for each ecological group

### aggregateEstimates.R 
* uses Standardized_Estimates_Wide.csv to
  + calculate the average performance (probability of persistence) under the Baseline scenario (Aggregated_Baseline.csv)
  + calculate benefit of each strategy, _Benefit = Strategy performance - Baseline performance_, and average the benefit across experts (Aggregated_Benefits.csv)
  + calculate the average performance (probability of persistence) under each strategy = _Aggregated_Benefits + Aggregated_Baseline_ (Aggregated_Performance.csv)
  
### getBenefitMatrix.R
* uses Aggregated_Baseline.csv and Aggregated_Benefits.csv, and a table of strategy Cost and Feasibility to
  + calculate the expected benefit of each strategy for each ecological group = _Benefit * Feasibility_ (Aggregated_Benefits_weighted.csv)
  + calculate the expected performance of each strategy for each ecological group = _Aggregated_Benefits_weighted + Aggregated_Baseline_ (Aggregated_Performance_weighted.csv)
  + create a Benefit matrix for use in the optimization using 'best guess' estimates from the Aggregated_Performance_weighted table (Benefits.csv)

### plotMeanPerformance.R
* can use Aggregated_Performance.csv or Aggregated_Performance_weighted.csv to 
  + create pointrange plots of (unweighted or weighted) standardized mean estimates of probability of persistence (y-axis) for each strategy (x-axis) and for each ecological group (subplots)

### calculateCEscore.R
* uses Aggregated_Benefits.csv and a table of strategy Cost and Feasibility to calculate a cost-effectiveness (CE) score  
_CE = (Benefit*Feasibility)/Cost_ and rank strategies by Benefit, Cost, and CE. Results are saved as Cost_Effectiveness.csv
 
### Sens_Analysis.R
* conducts uncertainty analysis for benefit estimates
