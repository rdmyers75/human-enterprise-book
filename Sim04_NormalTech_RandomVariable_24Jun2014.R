# Purpose: This script performs the simulation and produces the results
# shown in Figure 21 in the book Human Enterprise, Ecosystems, 
# and the Future of Civilization. In this simulation, there is "typical" 
# progress in "green technology". Random "disasters" are thrown in.
# The simulation runs for 1,000 years. 10 repetitions are performed. 
# Results will be different every time because no seed has been set 
# for the random number generator.

###################################################################################
# load libraries
library("ggplot2")
library("grid")
library("scales")
###################################################################################
# CONSTANTS AND AUXILIARY VARIABLES

# run controls
starting_year <- 1
ending_year <- 1000
disaster_toggle <- 1 # 0=no disasters; 1=disasters on
number_of_trials <- 10

# loop through for each trial
trial <- 0

while (trial < number_of_trials){
  
trial = trial + 1  
  
# constants
Rate_of_Increase_in_Human_Activity <- 0.02475 # /yr
Primary_Productivity <- 5.36e13 # kg C/yr
Replenishment_of_Natural_Capital <- Primary_Productivity
Unit_Depletion_of_Natural_Capital_t0 <- 5.5079e-08 # kg C / J/yr human activity
Rate_of_Decrease_in_Unit_Depletion_of_Natural_Capital <- 0.0025 # /yr
Effect_of_Natural_Capital_Depletion_on_Human_Activity <- 1.65e-16 # / kg C
Decrease_in_Human_Activity_t0 <- 0
Natural_Capital_Gap_t0 <- 0
disaster_multiplier_t0 <- 0

# initialize stocks
Natural_Capital_t0 <- 1.70e15
Natural_Capital_NCU_t0 <- 100

Human_Activity_t0 <- 5.84063e20

Human_Activity_W_t0 <- # human activity, W
  Human_Activity_t0 * # human activity, J/yr
  1 * # 1 W / J/s
  1 / (365 * 24 * 60 * 60) # 1 yr / (365 d/yr * 24 hr/d * 60 min/hr * 60 s/min

Kardashev_Scale_t0 <- (log(Human_Activity_W_t0, 10) - 6) / 10


###################################################################################


###################################################################################
# STOCK 1: Human_Activity
# Units: J/yr
# Initial Value: 
#   Human_Activity_t0 <- 5.84063e14
#
# Inflow: Increase_in_Human_Activity
# Units: J/yr/yr
# Relationship:
#   Human_Activity (J/yr) * Rate_of_Increase_in_Human_Activity (/yr)

# Outflow: Decrease_in_Human_Activity
# Units: J/yr/yr
# Relationship:
#   (Natural_Capital_t0, kg C - Natural_Capital, kg C) *
#     Effect_of_Natural_Capital_Depletion_on_Human_Activity, J/yr/yr / kg C)
#
###################################################################################
# STOCK 2: Natural_Capital
# Units: kg C
# Initial Value: 
# Natural_Capital_t0 <- 1.70e15
# Constraint: must be <= initial value
#
# Inflow: Replenishment_of_Natural_Capital
# Units: kg C/yr
# Relationship: = Primary_Productivity (kg C/yr)

# Outflow: Depletion_of_Natural_Capital
# Units: kg C/yr
# Relationship: 
#   Initial Value: Depletion_of_Natural_Capital_t0, (kg C/J/yr)
#   Thereafter: Effect_of_Natural_Capital_Depletion_on_Human_Activity(t-1), (kg C/J/yr) * 
#     (1 - Rate_of_Decrease_in_Depletion_of_Natural_Capital), (/yr) *
#     Human_Activity(t-1) (J/yr)

# Natural_Capital <- min(Natural_Capital, Natural_Capital_t0) 
#     
###################################################################################
# SIMULATION
#
# initialize values

year <- starting_year

Depletion_of_Natural_Capital_t0 <- 
  Unit_Depletion_of_Natural_Capital_t0 * Human_Activity_t0

Output_t0 <- c(trial,
            year, 
            Human_Activity_t0, 
            Human_Activity_W_t0,
            Kardashev_Scale_t0,
            Natural_Capital_t0,
            Natural_Capital_NCU_t0,
            Replenishment_of_Natural_Capital,
            Unit_Depletion_of_Natural_Capital_t0,
            Depletion_of_Natural_Capital_t0,
            Natural_Capital_Gap_t0,
            disaster_multiplier_t0,
            Decrease_in_Human_Activity_t0)

names(Output_t0) <- c("trial",
                  "Year", 
                  "Human_Activity", 
                  "Human_Activity_W",
                  "Kardashev_Scale",
                  "Natural_Capital",
                  "Natural_Capital_NCU",
                  "Replenishment_of_Natural_Capital",
                  "Unit_Depletion_of_Natural_Capital",
                  "Depletion_of_Natural_Capital",
                  "Natural_Capital_Gap",
                  "disaster_multiplier",
                  "Decrease_in_Human_Activity")

Human_Activity <- Human_Activity_t0
Human_Activity_W <- Human_Activity_W_t0
Natural_Capital <- Natural_Capital_t0
Unit_Depletion_of_Natural_Capital <- Unit_Depletion_of_Natural_Capital_t0

# for very first time through loop, create a new data frame for initial values
# for later times through loop, add a new row to the existing data set
if (trial > 1 && year == 1) {rbind(Output, Output_t0)}
if (trial == 1 && year ==1) {Output <- Output_t0}

# complete simulation
while (year < ending_year) {
  Output_tminus1 <- Output_t0
  year <- year + 1  
  
  # increment rate of natural capital depletion per unit human activity
  Unit_Depletion_of_Natural_Capital <- 
    Unit_Depletion_of_Natural_Capital *
    (1 - Rate_of_Decrease_in_Unit_Depletion_of_Natural_Capital)
  
  Depletion_of_Natural_Capital <- 
    Unit_Depletion_of_Natural_Capital * Human_Activity
  
  Natural_Capital_Gap <- Natural_Capital_t0 - Natural_Capital

  Decrease_in_Human_Activity <-
    Natural_Capital_Gap * 
    Effect_of_Natural_Capital_Depletion_on_Human_Activity *
    Human_Activity
  
  # decrease human activity in the event of a disaster
  # 1/100 chance of 5% reduction in activity
  # 1/200 chance of 30% reduction in activity
  # 1/500 chance of 50% reduction in activity
  # 1/1000 chance of 95% reduction in activity
  # 1/1e6 chance of 100% reduction in activity (extinction)
  disaster_multiplier <- 0  # default is no disaster
  rand_no <- runif(1,0,1)
    
  if (rand_no >= 0.99) disaster_multiplier <- 0.05
  if (rand_no >= 0.995) disaster_multiplier <- 0.30
  if (rand_no >= 0.998) disaster_multiplier <- 0.50
  if (rand_no >= 0.999) disaster_multiplier <- 0.95
  if (rand_no >= 0.999999) disaster_multiplier <- 1
  
  # disasters off if toggle is set to 0 by user
  disaster_multiplier <- disaster_multiplier * disaster_toggle
  
  Decrease_in_Human_Activity <- 
    Decrease_in_Human_Activity +
    (Human_Activity * 
       disaster_multiplier)
  
  # perform mass balance on stocks
  
  Human_Activity <-
    Human_Activity +
    Human_Activity * Rate_of_Increase_in_Human_Activity - 
    Decrease_in_Human_Activity

  Natural_Capital <-
    Natural_Capital +
    Replenishment_of_Natural_Capital - 
    Depletion_of_Natural_Capital
  
  # don't allow human activity to go negative
  Human_Activity = max(Human_Activity, 0)
  
  #constrain natural capital to less than or equal to initial value
  Natural_Capital <- min(Natural_Capital, Natural_Capital_t0)
  Natural_Capital <- max(Natural_Capital, 0)
  
  # define an arbitrary unit of natural capital
  Natural_Capital_NCU <- Natural_Capital * 100 / Natural_Capital_t0
  
  # calculate human activity in W and the Kardashev Scale
  Human_Activity_W <- # human activity, W
    Human_Activity * # human activity, J/yr
    1 * # 1 W / J/s
    1 / (365 * 24 * 60 * 60) # 1 yr / (365 d/yr * 24 hr/d * 60 min/hr * 60 s/min
  
  Kardashev_Scale <- (log(Human_Activity_W, 10) - 6) / 10
  
  # add newest values to output data set
  new_output <- c(trial,
                  year, 
                  Human_Activity, 
                  Human_Activity_W,
                  Kardashev_Scale,
                  Natural_Capital,
                  Natural_Capital_NCU,
                  Replenishment_of_Natural_Capital,
                  Unit_Depletion_of_Natural_Capital,
                  Depletion_of_Natural_Capital,
                  Natural_Capital_Gap,
                  disaster_multiplier,
                  Decrease_in_Human_Activity)
  
  names(new_output) <- c("trial",
                         "Year", 
                         "Human_Activity",
                         "Human_Activity_W",
                         "Kardashev_Scale",
                         "Natural_Capital",
                         "Natural_Capital_NCU",
                         "Replenishment_of_Natural_Capital",
                         "Unit_Depletion_of_Natural_Capital",
                         "Depletion_of_Natural_Capital",
                         "Natural_Capital_Gap",
                         "disaster_multiplier",
                         "Decrease_in_Human_Activity")

  Output <- rbind(Output, new_output)
}
}


###################################################################################

#Output # uncomment to print output data set if desired

# plot outputs of interest

# # plot human activity stock

  # convert from a matrix to a data frame (required for ggplot)
  Output_df <- as.data.frame(Output)
  
  # convert "trial" variable to a character to allow overlaying of plots
  Output_df$trial_char <- as.character(Output_df$trial)

  # PLOT HUMAN ACTIVITY (FIGURE 21)
  # assign data to be plotted
  Human_Activity_Plot <- ggplot(
    data = Output_df, # data set
    aes(x = Output_df$Year, # x variable
        y = Output_df$Human_Activity_W, # y variable
        color = Output_df$trial_char
    ),
  )
  
  # assign type of plot and any statistics to calculate
  Human_Activity_Plot <- Human_Activity_Plot + 
    geom_line( # a basic line
      stat = "identity", # plot the actual value, no statistical transformation
#      color = "blue", # set the line color
      size = 0.9 # set the line width
    )
  
  # assign a theme
  Human_Activity_Plot <- Human_Activity_Plot +
    theme_bw()  # a basic black and white theme
  
  # log transform axes
  Human_Activity_Plot <- Human_Activity_Plot +
    coord_trans(
      x = "log10", # transform x axis to log scale 
      y = "log10" # transform y axis to log scale
    )
  
  # set axis labels, scales, and intervals
  
  Human_Activity_Plot <- Human_Activity_Plot +
    scale_x_continuous(
      name = "Years", # x axis label
      limits = c(1, 1000), # set x axis limits
      breaks = c(5, 10, 50, 100, 500, 1000), # x axis limits and interval
      minor_breaks = function(x) log10(x)/10, # specify minor breaks (NOT DEBUGGED)
      expand = c(0.01,0) # control where y axis crosses - first number is fraction of plot left as white space
      #     labels = c("Years", "", "Decades", "", "Centuries", "") # replace x-axis numbers with text labels
    ) +
    scale_y_continuous(
      name = ("Human Activity (W)"), # y axis label
      limits = c(1e12, 1e15), # set y axis limits
      breaks = c(1e12, 1e13, 1e14, 1e15), # set breaks and tick marks
      expand = c(0.02,0) # control where x axis crosses - first number is fraction left as white space
    )
  
  # set font size of labels and numbers on axes
  
  Human_Activity_Plot <- Human_Activity_Plot +
    theme(
      axis.title.x = element_text(size = 18), # set font size of x axis label
      axis.text.x = element_text(size = 15), # set font size of x axis text
      axis.title.y = element_text(size = 18), # set font size of y axis label
      axis.text.y = element_text(size = 15), # set font size of y axis text
      panel.grid.minor = element_line(), # specify minor grid lines
	legend.position = "none"
    )
  
  # set tick marks
  
  Human_Activity_Plot <- Human_Activity_Plot +
    annotation_logticks( # adds tick marks to a log scale
      scaled = FALSE # tells R we have transformed the axis, not the data
    )    

Human_Activity_Plot