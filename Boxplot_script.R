## BOXPLOTS OF EXPERT ESTIMATES BY MANAGEMENT STRATEGY AND SPECIES GROUP

## This script produces graphs for each species group with three boxplots (best guess, lower, and upper estimates) for each management strategy.

## Adapted for the SJR PTM by Abbey Camaclang, 29 May 2019
## Requires output from combineTables.R, which organizes the estimates into a single table, 
## saved as "Results.csv" in the current working directory

# Load packages
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(sjPlot)
library(stringr)
library(RColorBrewer)

# Read in data
results <- read.csv("Results.csv")
# head(results)

# AC: need to standardize labels - should probably do it in combineTables.R instead
results$Ecological.Group <- as.character(results$Ecological.Group)
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest Species")] <- "Mature Forest and Peatland"
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest/ Peatland Species")] <- "Mature Forest and Peatland"
results$Ecological.Group[which(results$Ecological.Group=="Mature Forest/Peatland Species")] <- "Mature Forest and Peatland"

# MANIPULATE DATA TO LONG FORM
# Use tidyr package to transform data to long version, with single columns for Estimate (e.g., best guess, upper) and Value (value of estimate, 0-100)
rlong <-
  gather(results,
         key = Estimate,
         value = Value,
         Best.guess:Confidence_22) # AC: updated with the SJR data column names
# head(rlong)

# Drop NA values from the data (done here because this will remove rows from data that contain NA; before, when data was in wide form, a row could contain NA if an expert did not provide estimates for a strategy. Now, with data in long form, each individual estimate has it's own row, so when we omit NAs we are just deleting single estimates that were not filled out by experts  )
rlong <- na.omit(rlong)
write_csv(rlong, "Results_tidy.csv")

# TABLE of how many expert estimates there were for each species group
table.data <-spread(rlong, Estimate, Value) # Transform data back to long format (The long format was useful to drop species groups that experts didn't provide estimates for, but now we need to go back to wide format in order to table how many experts provided estimates for each species)
table.subset <- table.data[, c(1, 2)] # subset table.data to only include the columns "Expert" and "Ecological.Group" - this is the data we will table
exp.table <- table(table.subset$Ecological.Group)
write.csv(exp.table, "Estimates_per_group.csv", row.names=FALSE)

# CREATE SEPARATE COLUMNS FOR ESTIMATE TYPE AND STRATEGY #
# Add a column for Strategy labels
# Look for symbols that stand for strategies in the "Estimate" column, and fill the "Strategy" column with the appropriate strategies.
# AC: this has been updated to make it more generalizable (and so I won't have do each Strategy name separately). Basically, looks for the "_" character and uses the digits that follow it as the Strategy name. 
rlong$Strategy <- "BLANK"
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rlong$Estimate[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # AC: Rows without the "_" are Baseline estimates

# Create a new column for type of estimate (e.g. best guess).
rlong$Est.Type <- "BLANK" # creates a new column in the table #AC: not sure if this is really needed
rlong$Est.Type[grep("Best.guess", rlong$Estimate)] <- "Best.Guess"
rlong$Est.Type[grep("Lower", rlong$Estimate)] <- "Lower"
rlong$Est.Type[grep("Upper", rlong$Estimate)] <- "Upper"
rlong$Est.Type[grep("Confidence", rlong$Estimate)] <- "Confidence"


# PREP FOR STANDARDIZATION
# Subset the dataset include desired strategies (and remove strategies 11-13, the development strategies)
# AC: Commented out as we are looking at all strategies
# rlong <-
#   subset(
#     rlong,
#     Strategy %in% c(
#       "Baseline",
#       "S1",
#       "S2",
#       "S3",
#       "S4",
#       "S5",
#       "S6",
#       "S7",
#       "S8",
#       "S9",
#       "S10",
#       "S14",
#       "S15",
#       "S16",
#       "S17"
#     )
#   )

# TABLE # Table how many estimates there were for each species group
table.subset2 <- subset(rlong, Est.Type=="Best.Guess") # Subset to only include best guess estimates, just to count how many experts provided estimates for each group and strategy
#table.subset2 <- table.subset2[,c(2,5)] # subset to include desired columns # feel like this is unnecessary
strat.levels <- unique(table.subset2$Strategy) # AC: added to get the strategy names for use in specifying factor levels (so I won't have to type them out in the next line)
table.subset2$Strategy <- factor(table.subset2$Strategy,levels = strat.levels) # AC: specifies factor levels explicitly so that they stay in the right order (as.factor() changes the order to alphanumeric)

st.table <- table(table.subset2$Ecological.Group, table.subset2$Strategy)
# st.table
write.csv(st.table, "Estimates_by_strategy.csv")

# Subset the dataset to remove the "Estimate column" - it's not needed since we have separate columns for strategy and estimate type.
#rlong <- rlong[, -3] # commented out currently becaues I needed to transform the data to wide form at the end for Luara nd this was the easiest way to do it 
#head(rlong)

# Tell R to read the Value column as numbers not variables (it's currently the Value column as a set of characters because of the NAs that used to be there)
rlong$Value <- as.numeric((rlong$Value))
str(rlong) # check if it worked

# STANDARDIZE
# We are going to standardize upper and lower estimates to 80% confidence.
# Lower standardized interval: B−((B−L)×(S∕C))
# Upper standardized interval: B+((U−B)×(S∕C))
# ** I have checked these against hand calculations in excel and this method worked perfectly

# order dataframe by expert then est.type
# rlong[with(rlong, order(Est.Type, Expert, Strategy)),] # AC: I don't think this is doing anything, as it is still ordered by Strategy first, then Estimate, then expert

# subset dataframe by estimate type 
bg <- subset(rlong, Est.Type == "Best.Guess")
low <- subset(rlong, Est.Type == "Lower")
up <- subset(rlong, Est.Type == "Upper")
conf <- subset(rlong, Est.Type == "Confidence")

# AC: check that order of rows are the same, then comment out once checked
# results should equal the number of rows in tables, if all entries are matching
# test <- sum(str_detect(bg$Ecological.Group,low$Ecological.Group)) 
# test2 <- sum(str_detect(bg$Ecological.Group,up$Ecological.Group))
# test3 <- sum(str_detect(bg$Ecological.Group,conf$Ecological.Group))
# test4 <- sum(bg$Expert == low$Expert) # checks that Experts also align

# create new copies of the datasets we will apply the standardization to
low.new <- low
up.new <- up
bg.new <- bg
conf.new <- conf

# create new columns for standardized estimates and apply standardization to them (will come back and explain more later)

## STANDARDIZE
# so essentially what we need to do is:
# 1. Divide confidence by 100
# 2. For each expert, and each species group: take the upper and lower estimates, and standardize using
#       Lower standardized interval: B−((B−L)×(S∕C))
#       Upper standardized interval: B+((U−B)×(S∕C))
# 3. Leave best guess as is

## OLD STANDARDIZATION CODE: ###################################################################################
# AC: this assumes that they're all in the same order (i.e., by Strategy, Expert, and Ecol. Grp).
S <- 0.8 # confidence level to standardize estimates to
conf.new$St.Value <- (conf$Value) / 100
low.new$St.Value <-
  bg$Value - (((bg$Value) - (low$Value)) * (S / (conf.new$St.Value)))
up.new$St.Value <-
  bg$Value + (((up$Value) - (bg$Value)) * (S / (conf.new$St.Value)))
bg.new$St.Value <- bg$Value

# Bind subsets together back together into a new table
rlong.std <- rbind(low.new, up.new, bg.new, conf.new) 
# rlong.std

# Change any negative standardized values to zero
rlong.std$St.Value[rlong.std$St.Value < 0] <- 0
rlong.std$St.Value[rlong.std$St.Value > 100] <- 100

rlong.sub <- rlong.std[,c(1,2,3,7)] # subsets only relevant columns
# rlong.sub<- rlong.std[with(rlong.std, order(Strategy)),]

# Save standardized estimates into a new table, with same format as results
est.levels <- unique(rlong$Estimate)
rlong.wide <- spread(rlong.sub, Estimate, St.Value)
rlong.wide <- rlong.wide[, c("Expert", "Ecological.Group", est.levels)] # AC: Reorders columns -- Does the same as before (in FRE code), but more generalizable (and less typing)

grp.levels <- unique(rlong.std$Ecological.Group)
rlong.wide$Ecological.Group<-factor(rlong.wide$Ecological.Group, levels=grp.levels) # AC: does the same as before

## Table that produces how many experts estimated for which species group
rlong.wide<-with(rlong.wide, rlong.wide[order(Expert, Ecological.Group),]) # AC: Added to arrange rows in same order as results table
write_csv(rlong.wide, "Standardized_Estimates_Wide.csv") # export to .csv


## PLOTTING CODE: ##############################################################################################

# ORDER DATASET & SUBSET FOR PLOTTING
# Order the levels of the dataframe so that the strategies are plotted in the desired order

rlong.std$Strategy <- factor(rlong.std$Strategy,levels = strat.levels)

# Subset to remove confidence estimates
rlong.std <- subset(rlong.std, Est.Type %in% c("Best.Guess", "Lower", "Upper"))

# renaming Grassland/Open Habitat Species as it doesn't work as a filename as is (Should do this in combineTables.R instead)
grp.names <- grp.levels
grp.names[which(grp.levels=="Grassland/Open Habitat species")] <- paste0("Grassland Species")

# Plot group estimates as boxplots
for (i in seq_along(grp.levels)) {
  temp.grpdata <- subset(rlong.std, Ecological.Group == grp.levels[i])
  plot1 <-
    ggplot(temp.grpdata, aes(x = Est.Type, y = St.Value, fill = Est.Type)) + # using the data Ecological group, graph Estimate Type on x-axis and St.Value on y-axis, and colour the boxplots by estimate type
    geom_boxplot() +                                 # tell R to display data in boxplot form
    # geom_point(
    #   data = subset(temp.grpdata, Expert == "1"),
    #   # tell R to graph Expert 1's estimates for the ecol grp as red datapoints on top of the boxplots (keeping Estimate Type on x-axis and St.Value on y-axis)
    #   aes(x = Est.Type, y = St.Value),
    #   color = 'red'
    # ) +
    theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
    theme(
      plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
      # adjust margins around the outside of the plot (bottom=0,left=1,top=0,right=0.5)
      panel.spacing = unit(1, "lines"),
      # adjust margins and between panels of the plot (spacing of 1)
      axis.title.y = element_text(margin = margin(
        t = 0,
        r = 10,
        b = 0,
        l = 0
      ))
    ) + # adjust space between y-axis numbers and y-axis label
    facet_wrap( ~ Strategy, nrow = 3) +  # tell R to create a separate panel of estimates for each management strategy
    scale_x_discrete(
      name = "",
      breaks = c("Best.Guess", "Lower", "Upper"),
      labels = c("B", "L", "U")
    ) + # Give the x-axis variables shortened labels
    scale_fill_manual(values = c("gray80", "white", "white"),
                      guide = FALSE) + # Assign colours to each type of estimate and don't show a legend
    labs(x = "", y = "Probability of persistence (%)", title = paste(grp.levels[i])) +  # put a horizontal label for the species group on the y axis
    ylim(0, 100) # set the y-axis limits from 0-100
  
  # plot1 # print the plot onscreen
  
  # AC: save plots as .pdf, one file per ecological group
  ggsave(plot1, file=paste0(grp.names[i], ".pdf", sep=''), width = 10, height = 8, units = "in")
}

## PLOT each expert estimate separately (x-axis = Expert, y-axis point = Best guess, range = lower->upper)
rlong.sub2 <- rlong.std[,c(1,2,5,6,7)]
rlong.std.wide <- spread(rlong.sub2,key=Est.Type,value=St.Value)
rlong.std.wide$Expert<-as.factor(rlong.std.wide$Expert)

for (i in seq_along(grp.levels)) {
  temp.expdata <- subset(rlong.std.wide, Ecological.Group == grp.levels[i])
  plot2 <-
    ggplot(temp.expdata, aes(x = Expert, y = Best.Guess, color = Expert)) + # using the data Ecological group, graph Estimate Type on x-axis and St.Value on y-axis, and colour the boxplots by estimate type
    geom_pointrange(aes(ymin = Lower, ymax = Upper))+
    theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
    theme(
      plot.margin = unit(c(0, 1, 0, 0.75), "cm"), # adjust margins around the outside of the plot (bottom=0,left=1,top=0,right=0.5)
      panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
      # panel.background = element_rect(fill='gray'),
      axis.title.y = element_text(margin = margin(
        t = 0,
        r = 10,
        b = 0,
        l = 0
      )),
      axis.text.x = element_blank(),
      legend.justification=c(1,0), legend.position=c(0.98,-0.05)
    ) + # adjust space between y-axis numbers and y-axis label, removes x-axis tick labels, repositions legend box
    # scale_color_brewer(palette='Paired') + # changes color palette
    facet_wrap( ~ Strategy, nrow = 3) +  # tell R to create a separate panel of estimates for each management strategy
    labs(x = "Experts", y = "Probability of persistence (%)", title = paste(grp.levels[i])) +  # put a horizontal label for the species group on the y axis
    ylim(0, 100) # set the y-axis limits from 0-100
  
  # plot1 # print the plot onscreen
  
  # AC: save plots as .pdf, one file per ecological group
  ggsave(plot2, file=paste0(grp.names[i],"_byExp.pdf", sep=''), width = 10, height = 8, units = "in")
}

### END ###

### old FRE PTM code for generating plots manually:
# AC: Commented out as I've looped it instead (above)

# # Subset by species group
# pel.sea <- subset(rlong, Ecological.Group == "Pelagic Seabirds")
# raptors <-
#   subset(rlong, Ecological.Group == "Diurnal & Nocturnal Raptors")
# songbirds <- subset(rlong, Ecological.Group == "Songbirds, Migrants")
# an.fish <- subset(rlong, Ecological.Group == "Fish, Anadromous")
# bats <- subset(rlong, Ecological.Group == "Aerial Insectivore, Mammal")
# orca <- subset(rlong, Ecological.Group == "Marine Mammal")
# sand <- subset(rlong, Ecological.Group == "Coastal Sand Ecosystems")
# grass <-
#   subset(rlong,
#          Ecological.Group == "Grassland-threatened Species, Migrants")
# forest <- subset(rlong, Ecological.Group == "Forest Specialists")
# rip.fresh <-
#   subset(rlong,
#          Ecological.Group == "Riparian: Freshwater (marsh, creek, mudflat, lake)")
# rip.salt <-
#   subset(rlong,
#          Ecological.Group == "Riparian: Saltwater (Mudflat/seagrass/saltmarsh)")
# wet.res <-
#   subset(rlong,
#          Ecological.Group == "Wetland-threatened Species, Residents")
# wet.mig <-
#   subset(rlong,
#          Ecological.Group == "Wetland-threatened Species, Migrants")
# 
# # PLOT
# # Pelagic Seabirds
# plot1 <-
#   ggplot(pel.sea, aes(x = Est.Type, y = St.Value, fill = Est.Type)) + # using the data on pelagic seabirds (pel.sea), graph Estimate Type on x-axis and St.Value on y-axis, and colour the boxplots by estimate type
#   geom_boxplot() +                                 # tell R to display data in boxplot form
#   geom_point(
#     data = subset(pel.sea, Expert == "28"),
#     # tell R to graph Expert 1's pelagic seabird estimates as red datapoints on top of the boxplots (keeping Estimate Type on x-axis and St.Value on y-axis)
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
#     # adjust margins around the outside of the plot (bottom=0,left=1,top=0,right=0.5)
#     panel.spacing = unit(1, "lines"),
#     # adjust margins and between panels of the plot (spacing of 1)
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 10,
#       b = 0,
#       l = 0
#     ))
#   ) + # adjust space between y-axis numbers and y-axis label
#   facet_wrap( ~ Strategy, nrow = 1) +  # tell R to create a separate panel of estimates for each management strategy
#   scale_x_discrete(
#     name = "",
#     breaks = c("Best.Guess", "Lower", "Upper"),
#     labels = c("B", "L", "U")
#   ) + # Give the x-axis variables shortened labels
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) + # Assign colours to each type of estimate and don't show a legend
#   labs(x = "", y = "Seabirds", title = "") +  # put a horizontal label for the species group on the y axis
#   ylim(0, 100) # set the y-axis limits from 0-100
# #plot1 # print the plot
# 
# # Diurnal & Nocturnal Raptors
# plot2 <-
#   ggplot(raptors, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(raptors, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 10,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Diurnal & Nocturnal Raptors", title = "") +
#   ylim(0, 100)
# #plot2
# 
# # Songbirds, Migrants
# plot3 <-
#   ggplot(songbirds, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(songbirds, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 10,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Songbirds, Migrants", title = "") +
#   ylim(0, 100)
# #plot3
# 
# # Fish, Anadromous
# plot4 <-
#   ggplot(an.fish, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(an.fish, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 10,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Fish, Anadromous", title = "") +
#   ylim(0, 100)
# #plot4
# 
# # Aerial Insectivores, Mammals
# plot5 <- ggplot(bats, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(data = subset(bats, Expert == "28"),
#              aes(x = Est.Type, y = St.Value),
#              color = 'red') +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 0.3,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Aerial Insectivores,\nMammals", title = "") +
#   ylim(0, 100)
# #plot5
# 
# # Marine Mammal
# plot6 <- ggplot(orca, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(data = subset(orca, Expert == "28"),
#              aes(x = Est.Type, y = St.Value),
#              color = 'red') +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 10,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Marine Mammal", title = "") +
#   ylim(0, 100)
# #plot6
# 
# # Coastal Sand Ecosystems
# plot7 <- ggplot(sand, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(data = subset(sand, Expert == "28"),
#              aes(x = Est.Type, y = St.Value),
#              color = 'red') +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 0.3,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Coastal Sand\n Ecosystems", title = "") +
#   ylim(0, 100)
# #plot7
# 
# # Grassland-threatend Species, Migrants
# plot8 <- ggplot(grass, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(grass, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 0.3,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Grassland-threatened\nSpecies, Migrants", title = "") +
#   ylim(0, 100)
# #plot8
# 
# # Forest Specialists
# plot9 <- ggplot(forest, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(forest, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 10,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Forest Specialsts", title = "") +
#   ylim(0, 100)
# #plot9
# 
# # Riparian: Freshwater
# plot10 <-
#   ggplot(rip.fresh, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(rip.fresh, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 10,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Riparian: Freshwater", title = "") +
#   ylim(0, 100)
# #plot10
# 
# # Riparian: Saltwater
# plot11 <-
#   ggplot(rip.salt, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(rip.salt, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 10,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Riparian: Saltwater", title = "") +
#   ylim(0, 100)
# #plot11
# 
# # Wetland-threatened Species, Residents
# plot12 <-
#   ggplot(wet.res, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(wet.res, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 0.3,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Wetland-threatened\nSpecies, Residents", title = "") +
#   ylim(0, 100)
# #plot12
# 
# # Wetland-threatened Species, Migrants
# plot13 <-
#   ggplot(wet.mig, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
#   geom_boxplot() +
#   geom_point(
#     data = subset(wet.mig, Expert == "28"),
#     aes(x = Est.Type, y = St.Value),
#     color = 'red'
#   ) +
#   theme_cowplot() +
#   theme(
#     plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
#     panel.spacing = unit(1, "lines"),
#     axis.title.y = element_text(margin = margin(
#       t = 0,
#       r = 0.3,
#       b = 0,
#       l = 0
#     ))
#   ) +
#   facet_wrap( ~ Strategy, nrow = 1) +
#   scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
#                    labels = c("B", "L", "U")) +
#   scale_fill_manual(values = c("gray80", "white", "white"),
#                     guide = FALSE) +
#   labs(x = "", y = "Wetland-threatened\nSpecies, Migrants", title = "") +
#   ylim(0, 100)
# #plot13
# 
# # PLOT THE FIRST FIVE GRAPHS
# plot_grid(plot1, plot2, plot3, nrow = 3)
# 
# # Laura needs them readable. 10 species groups. If there is a way to loop it for 18 experts then 
# # Species groups BA1 - BA10 for the species name
# # Find a way to layer them. 15 strategies. 
# 
# plot_grid(plot4, plot5, plot6,
#           ncol = 1, nrow = 3)
# 
# plot_grid(plot7, plot8, plot9,
#           ncol = 1, nrow = 3)
# 
# plot_grid(plot10, plot11, plot12,
#           ncol = 1, nrow = 3)
# 
# plot_grid(plot13,
#           ncol = 1, nrow = 3)