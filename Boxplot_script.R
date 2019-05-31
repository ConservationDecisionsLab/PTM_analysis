## BOXPLOTS OF EXPERT ESTIMATES BY MANAGEMENT STRATEGY AND SPECIES GROUP

## This script produces graphs for each species group with three boxplots (best guess, lower, and upper estimates) for each management strategy.
## The data for this script comes from the "R Dataset" tab of our "FRE_benefits_biodiversity_template_plots" master data file, in the 02_Data folder of our project Dropbox
## The "R Dataset" tab of our master data file was saved as as a .csv file called "Results.csv" so that the data could be read into R. "Results.csv" can be found in our project dropbox under 09_Plot_Scripts > Final_Data_Scripts.
## *** NOTE: when entering expert data into the "R Dataset tab of our master data file, make sure to sort by ecological group and drag down the ecological group names (i.e. click on the first cell that says "Fish, Anadromous", and drag it down for all the "Fish, Anadroous" rows. This will ensure no expert estimates are subsetted out later in the code)
## **** "Results.csv" must be saved to your working directory in order for the code below to work ****


# Load packages
library(tidyr)
library(ggplot2)
library (cowplot)
library (gridExtra)
library (sjPlot)


# Read in data
results <- read.csv("Results.csv")
head(results)


# MANIPULATE DATA TO LONG FORM
# Use tidyr package to transform data to long version, with single columns for Estimate (e.g., best guess, upper) and Value (value of estimate, 0-100)
rlong <-
  gather(results,
         key = Estimate,
         value = Value,
         B.Best.Guess:S17.Confidence)
head(rlong)

# Drop NA values from the data (done here because this will remove rows from data that contain NA; before, when data was in wide form, a row could contain NA if an expert did not provide estimates for a strategy. Now, with data in long form, each individual estimate has it's own row, so when we omit NAs we are just deleting single estimates that were not filled out by experts  )
rlong <- na.omit(rlong)
head(rlong)
write.csv(rlong, "Rlong_test.csv")

# TABLE # Table how many estimates tehre were for each species group
table.data <-spread(rlong, Estimate, Value) # Transform data back to long format (The long format was useful to drop species groups that experts didn't provide estimates for, but now we need to go back to wide format in order to table how many experts provided estimates for each species)
head(table.data) # check the wide format is correct
  table.data[, c(1, 2)] # subset table.data to only include the columns "Expert" and "Ecological.Group" - this is the data we will table
head(table.subset) # check subsetting is correct
exp.table <-
  table(table.subset$Ecological.Group) # Create a table that shows number of estimates that provided estimates for each species group
write.csv(exp.table, "estimates_per_spp_group.csv")


# CREATE SEPERATE COLUMNS FOR ESTIMATE TYPE AND STRATEGY #
# Because of the way the data was set up in excel, type of estimate (e.g. best guess) and strategy # (e.g. strategy 1) are currently combined into one variable (for example, the best guess for Strategy 1 is called S1.Best.Guess)
# We now have to create two new columns to seperate out the type of estimate (e.g. best guess) and the strategy # (e.g. strategy 1)

# First, we add a column to the dataset called "Strategy" which will contain the names for our 14 Management Strategies. For now, fill this column with "BLANK".
rlong$Strategy <- "BLANK"
head(rlong)

# Second, we tell R to look for symbols that stand for strategies in the "Estimate" column, and fill the "Strategy" column with the appropriate strategies.
# We do this because the "Estimate" column currently combines the strategy # and type of estimate into one variable (e.g. S1.Best.Guess)
rlong$Strategy[grep("B.", rlong$Estimate)] <- "Baseline"
rlong$Strategy[grep("S1", rlong$Estimate)] <- "S1"
rlong$Strategy[grep("S2", rlong$Estimate)] <- "S2"
rlong$Strategy[grep("S3", rlong$Estimate)] <- "S3"
rlong$Strategy[grep("S4", rlong$Estimate)] <- "S4"
rlong$Strategy[grep("S5", rlong$Estimate)] <- "S5"
rlong$Strategy[grep("S6", rlong$Estimate)] <- "S6"
rlong$Strategy[grep("S7", rlong$Estimate)] <- "S7"
rlong$Strategy[grep("S8", rlong$Estimate)] <- "S8"
rlong$Strategy[grep("S9", rlong$Estimate)] <- "S9"
rlong$Strategy[grep("S10", rlong$Estimate)] <- "S10"
rlong$Strategy[grep("S11", rlong$Estimate)] <- "S11"
rlong$Strategy[grep("S12", rlong$Estimate)] <- "S12"
rlong$Strategy[grep("S13", rlong$Estimate)] <- "S13"
rlong$Strategy[grep("S14", rlong$Estimate)] <- "S14"
rlong$Strategy[grep("S15", rlong$Estimate)] <- "S15"
rlong$Strategy[grep("S16", rlong$Estimate)] <- "S16"
rlong$Strategy[grep("S17", rlong$Estimate)] <- "S17"

# The dataset now has a column for strategy
head(rlong)

# Third, create a new column for type of estimate (e.g. best guess). Call it "Est.Type" and fill it with blank cells for now.
rlong$Est.Type <- "BLANK"
head(rlong)

# Fourth,tell R to look for the type of estimate in the "Estimate" column, and fill the "Est.Type" column with the appropriate names.
# We do this because the "Estimate" column currently combines the strategy # and type of estimate into one variable (e.g. S1.Best.Guess)
rlong$Est.Type[grep("Best.Guess", rlong$Estimate)] <- "Best.Guess"
rlong$Est.Type[grep("Lower", rlong$Estimate)] <- "Lower"
rlong$Est.Type[grep("Upper", rlong$Estimate)] <- "Upper"
rlong$Est.Type[grep("Confidence", rlong$Estimate)] <- "Confidence"


# Final product: one dataset with separate columns for strategy and estimate type
head(rlong)


# PREP FOR STANDARDIZATION
# Subset the dataset include desired strategies (and remove strategies 11-13, the development strategies)
rlong <-
  subset(
    rlong,
    Strategy %in% c(
      "Baseline",
      "S1",
      "S2",
      "S3",
      "S4",
      "S5",
      "S6",
      "S7",
      "S8",
      "S9",
      "S10",
      "S14",
      "S15",
      "S16",
      "S17"
    )
  )

# TABLE # Table how many estimates there were for each species group
table.subset2<-subset(rlong, Est.Type=="Lower") # Subset to only include lower estimates (we are counting how many estimates were given for each species group AND each strategy. In wide form, our dataset is currently set up tp lump estimate type and strategy # into one variable, rather than having estimate type as a separate variable than strategy #. This makes it difficult to count whether an expert estiamted a strategy. In long form, there are four replications of each strategy # per expert (one for Lower, Upper, Best Guess, Confidence), which also makes it difficult to count the how many experts estimated for each strategy. What I've done here is to subsett the long form to only include one estimate type (Lower). I did this beacuse if an expert estimated a strategy, they would have provided a lower estimate, so counting lower estimates is the same as counting the # of experts that estiamted for each strategy. This overcomes the problem of counting estimates in long form.
table.subset2<-table.subset2[,c(2,4)] # subset to include desired columns
table.subset2<-
  factor(
    table.subset2$Strategy,
    levels = c(
      "Baseline",
      "S1",
      "S2",
      "S3",
      "S4",
      "S5",
      "S6",
      "S7",
      "S8",
      "S9",
      "S10",
      "S11",
      "S12",
      "S13",
      "S14",
      "S15",
      "S16",
      "S17"
    )
  )


st.table<-table(table.subset2$Strategy)
st.table
write.csv(st.table, "Number Estimates by Strategy.csv")





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
rlong[with(rlong, order(Est.Type, Expert, Strategy)),]

# subset dataframe by estimate type
bg <- subset(rlong, Est.Type == "Best.Guess")
low <- subset(rlong, Est.Type == "Lower")
up <- subset(rlong, Est.Type == "Upper")
conf <- subset(rlong, Est.Type == "Confidence")

# create new copies of the datasets we will apply the standardization to
low.new <- low
up.new <- up
bg.new <- bg
conf.new <- conf

# create new columns for standardized estimates and apply satndardization to them (will come back and explain more later)

## STANDARDIZE
# so essentially what we need to do is:
# 1. Divide confidence by 100
# 2. For each expert, and each species group: take the upper and lower estimates, and standardize using
#       Lower standardized interval: B−((B−L)×(S∕C))
#       Upper standardized interval: B+((U−B)×(S∕C))
# 3. Leave best guess as is

## OLD STANDARDIZATION CODE: ###################################################################################
conf.new$St.Value <- (conf$Value) / 100
low.new$St.Value <-
  bg$Value - (((bg$Value) - (low$Value)) * (0.80 / (conf.new$St.Value)))
up.new$St.Value <-
  bg$Value + (((up$Value) - (bg$Value)) * (0.80 / (conf.new$St.Value)))
bg.new$St.Value <- bg$Value

# now bind the subsets together back into the original dataframe
rlong <- rbind(low.new, up.new, bg.new, conf.new)
rlong

# now change any negative standardized values to zero
rlong$St.Value[rlong$St.Value < 0] <- 0
rlong$St.Value[rlong$St.Value > 100] <- 100
head(rlong)
rlong.sub<-rlong[,c(1,2,3,7)]
head(rlong.sub)
rlong.wide<-spread(rlong.sub, Estimate, St.Value)
head(rlong.wide)

rlong.wide<-rlong.wide[,c("Expert", "Ecological.Group", "B.Best.Guess",	"B.Lower",	"B.Upper", "B.Confidence", # Re-order the columns int he dataset so they reflect the order that we have in excel 
                                                          "S1.Best.Guess",	"S1.Lower",	"S1.Upper","S1.Confidence",
                                                          "S2.Best.Guess",  "S2.Lower",	"S2.Upper","S2.Confidence",
                                                          "S3.Best.Guess",	"S3.Lower",	"S3.Upper","S3.Confidence",
                                                          "S4.Best.Guess",	"S4.Lower",	"S4.Upper", "S4.Confidence",
                                                          "S5.Best.Guess",	"S5.Lower",	"S5.Upper", "S5.Confidence",
                                                          "S6.Best.Guess",	"S6.Lower",	"S6.Upper", "S6.Confidence",
                                                          "S7.Best.Guess",	"S7.Lower",	"S7.Upper", "S7.Confidence",
                                                          "S8.Best.Guess",	"S8.Lower",	"S8.Upper", "S8.Confidence",
                                                          "S9.Best.Guess",	"S9.Lower",	"S9.Upper", "S9.Confidence",
                                                          "S10.Best.Guess",	"S10.Lower",	"S10.Upper", "S10.Confidence",
                                                          "S14.Best.Guess",	"S14.Lower",	"S14.Upper", "S14.Confidence",
                                                          "S15.Best.Guess",	"S15.Lower",	"S15.Upper", "S15.Confidence",
                                                          "S16.Best.Guess",	"S16.Lower",	"S16.Upper", "S16.Confidence",
                                                          "S17.Best.Guess",	"S17.Lower",	"S17.Upper", "S17.Confidence")]

rlong.wide$Ecological.Group<-factor(rlong.wide$Ecological.Group, levels=c("Pelagic Seabirds",
                                                         "Diurnal & Nocturnal Raptors",
                                                         "Songbirds, Migrants",
                                                         "Fish, Anadromous",
                                                         "Aerial Insectivore, Mammal",
                                                         "Marine Mammal",
                                                         "Coastal Sand Ecosystems",
                                                         "Grassland-threatened Species, Migrants",
                                                         "Forest Specialists",
                                                         "Riparian: Freshwater (marsh, creek, mudflat, lake)",
                                                         "Riparian: Saltwater (Mudflat/seagrass/saltmarsh)",
                                                         "Wetland-threatened Species, Residents", 
                                                         "Wetland-threatened Species, Migrants"))

levels(rlong.wide$Ecological.Group)
head(rlong.wide)
write.csv(rlong.wide, "Standardized_Estimates_Wide.csv") # export to .csv


## Table that produces how many experts estimated for which species group



## PLOTTING CODE: ##############################################################################################
## ** SHOULD PROBABLY RENAME THE DATASET USED IN THIS SO EVERYTHING IS CLEAR **

# ORDER DATASET & SUBSET FOR PLOTTING
# Order the levels of the dataframe so that the strategies are plotted in the desired order
rlong$Strategy <-
  factor(
    rlong$Strategy,
    levels = c(
      "Baseline",
      "S1",
      "S2",
      "S3",
      "S4",
      "S5",
      "S6",
      "S7",
      "S8",
      "S9",
      "S10",
      "S11",
      "S12",
      "S13",
      "S14",
      "S15",
      "S16",
      "S17"
    )
  )

# Subset to remove confidence estiamtes
rlong <- subset(rlong, Est.Type %in% c("Best.Guess", "Lower", "Upper"))

# Subset by species group
pel.sea <- subset(rlong, Ecological.Group == "Pelagic Seabirds")
raptors <-
  subset(rlong, Ecological.Group == "Diurnal & Nocturnal Raptors")
songbirds <- subset(rlong, Ecological.Group == "Songbirds, Migrants")
an.fish <- subset(rlong, Ecological.Group == "Fish, Anadromous")
bats <- subset(rlong, Ecological.Group == "Aerial Insectivore, Mammal")
orca <- subset(rlong, Ecological.Group == "Marine Mammal")
sand <- subset(rlong, Ecological.Group == "Coastal Sand Ecosystems")
grass <-
  subset(rlong,
         Ecological.Group == "Grassland-threatened Species, Migrants")
forest <- subset(rlong, Ecological.Group == "Forest Specialists")
rip.fresh <-
  subset(rlong,
         Ecological.Group == "Riparian: Freshwater (marsh, creek, mudflat, lake)")
rip.salt <-
  subset(rlong,
         Ecological.Group == "Riparian: Saltwater (Mudflat/seagrass/saltmarsh)")
wet.res <-
  subset(rlong,
         Ecological.Group == "Wetland-threatened Species, Residents")
wet.mig <-
  subset(rlong,
         Ecological.Group == "Wetland-threatened Species, Migrants")

# PLOT
# Pelagic Seabirds
plot1 <-
  ggplot(pel.sea, aes(x = Est.Type, y = St.Value, fill = Est.Type)) + # using the data on pelagic seabirds (pel.sea), graph Estimate Type on x-axis and St.Value on y-axis, and colour the boxplots by estimate type
  geom_boxplot() +                                 # tell R to display data in boxplot form
  geom_point(
    data = subset(pel.sea, Expert == "28"),
    # tell R to graph Expert 1's pelagic seabird estimates as red datapoints on top of the boxplots (keeping Estimate Type on x-axis and St.Value on y-axis)
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
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
  facet_wrap( ~ Strategy, nrow = 1) +  # tell R to create a separate panel of estimates for each management strategy
  scale_x_discrete(
    name = "",
    breaks = c("Best.Guess", "Lower", "Upper"),
    labels = c("B", "L", "U")
  ) + # Give the x-axis variables shortened labels
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) + # Assign colours to each type of estimate and don't show a legend
  labs(x = "", y = "Seabirds", title = "") +  # put a horizontal label for the species group on the y axis
  ylim(0, 100) # set the y-axis limits from 0-100
#plot1 # print the plot

# Diurnal & Nocturnal Raptors
plot2 <-
  ggplot(raptors, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(raptors, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Diurnal & Nocturnal Raptors", title = "") +
  ylim(0, 100)
#plot2

# Songbirds, Migrants
plot3 <-
  ggplot(songbirds, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(songbirds, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Songbirds, Migrants", title = "") +
  ylim(0, 100)
#plot3

# Fish, Anadromous
plot4 <-
  ggplot(an.fish, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(an.fish, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Fish, Anadromous", title = "") +
  ylim(0, 100)
#plot4

# Aerial Insectivores, Mammals
plot5 <- ggplot(bats, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(data = subset(bats, Expert == "28"),
             aes(x = Est.Type, y = St.Value),
             color = 'red') +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 0.3,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Aerial Insectivores,\nMammals", title = "") +
  ylim(0, 100)
#plot5

# Marine Mammal
plot6 <- ggplot(orca, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(data = subset(orca, Expert == "28"),
             aes(x = Est.Type, y = St.Value),
             color = 'red') +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Marine Mammal", title = "") +
  ylim(0, 100)
#plot6

# Coastal Sand Ecosystems
plot7 <- ggplot(sand, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(data = subset(sand, Expert == "28"),
             aes(x = Est.Type, y = St.Value),
             color = 'red') +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 0.3,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Coastal Sand\n Ecosystems", title = "") +
  ylim(0, 100)
#plot7

# Grassland-threatend Species, Migrants
plot8 <- ggplot(grass, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(grass, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 0.3,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Grassland-threatened\nSpecies, Migrants", title = "") +
  ylim(0, 100)
#plot8

# Forest Specialists
plot9 <- ggplot(forest, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(forest, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Forest Specialsts", title = "") +
  ylim(0, 100)
#plot9

# Riparian: Freshwater
plot10 <-
  ggplot(rip.fresh, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(rip.fresh, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Riparian: Freshwater", title = "") +
  ylim(0, 100)
#plot10

# Riparian: Saltwater
plot11 <-
  ggplot(rip.salt, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(rip.salt, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.75), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Riparian: Saltwater", title = "") +
  ylim(0, 100)
#plot11

# Wetland-threatened Species, Residents
plot12 <-
  ggplot(wet.res, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(wet.res, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 0.3,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Wetland-threatened\nSpecies, Residents", title = "") +
  ylim(0, 100)
#plot12

# Wetland-threatened Species, Migrants
plot13 <-
  ggplot(wet.mig, aes(x = Est.Type, y = St.Value, fill = Est.Type)) +
  geom_boxplot() +
  geom_point(
    data = subset(wet.mig, Expert == "28"),
    aes(x = Est.Type, y = St.Value),
    color = 'red'
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(0, 1, 0, 0.5), "cm"),
    panel.spacing = unit(1, "lines"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 0.3,
      b = 0,
      l = 0
    ))
  ) +
  facet_wrap( ~ Strategy, nrow = 1) +
  scale_x_discrete(breaks = c("Best.Guess",  "Lower", "Upper"),
                   labels = c("B", "L", "U")) +
  scale_fill_manual(values = c("gray80", "white", "white"),
                    guide = FALSE) +
  labs(x = "", y = "Wetland-threatened\nSpecies, Migrants", title = "") +
  ylim(0, 100)
#plot13

# PLOT THE FIRST FIVE GRAPHS
plot_grid(plot1, plot2, plot3, nrow = 3)

# Laura needs them readable. 10 species groups. If there is a way to loop it for 18 experts then 
# Species groups BA1 - BA10 for the species name
# Find a way to layer them. 15 strategies. 

plot_grid(plot4, plot5, plot6,
          ncol = 1, nrow = 3)

plot_grid(plot7, plot8, plot9,
          ncol = 1, nrow = 3)

plot_grid(plot10, plot11, plot12,
          ncol = 1, nrow = 3)

plot_grid(plot13,
          ncol = 1, nrow = 3)