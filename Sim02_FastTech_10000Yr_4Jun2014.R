# Simulation: Sim02_FastTech_4Jun2014
# Description: green tech progress faster than overall growth
# last modified: by Dwayne June 4, 2014

###################################################################################
# load libraries
library("ggplot2")
library("grid")
library("scales")
###################################################################################
# CONSTANTS AND AUXILIARY VARIABLES

# run controls
starting_year <- 1
ending_year <- 10000

# constants
Rate_of_Increase_in_Human_Activity <- 0.02475 # /yr
Primary_Productivity <- 5.36e13 # kg C/yr
Replenishment_of_Natural_Capital <- Primary_Productivity
Unit_Depletion_of_Natural_Capital_t0 <- 5.5079e-08 # kg C / J/yr human activity
# Rate_of_Decrease_in_Unit_Depletion_of_Natural_Capital <- 0.0025 # /yr
Rate_of_Decrease_in_Unit_Depletion_of_Natural_Capital <- 0.03 # /yr
Effect_of_Natural_Capital_Depletion_on_Human_Activity <- 1.65e-16 # / kg C
Decrease_in_Human_Activity_t0 <- 0
Natural_Capital_Gap_t0 <- 0

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

Output <- c(year, 
            Human_Activity_t0, 
            Human_Activity_W_t0,
            Kardashev_Scale_t0,
            Natural_Capital_t0,
            Natural_Capital_NCU_t0,
            Replenishment_of_Natural_Capital,
            Unit_Depletion_of_Natural_Capital_t0,
            Depletion_of_Natural_Capital_t0,
            Natural_Capital_Gap_t0,
            Decrease_in_Human_Activity_t0)

# names(Output) <- c("Year", 
#                        "Human_Activity", 
#                        "Kardashev_Scale",
#                        "Natural_Capital",
#                        "Natural_Capital_NCU",
#                        "Replenishment_of_Natural_Capital",
#                        "Unit_Depletion_of_Natural_Capital",
#                        "Depletion_of_Natural_Capital",
#                        "Natural_Capital_Gap",
#                        "Decrease_in_Human_Activity")

Human_Activity <- Human_Activity_t0
Natural_Capital <- Natural_Capital_t0
Unit_Depletion_of_Natural_Capital <- Unit_Depletion_of_Natural_Capital_t0

# complete simulation
while (year < ending_year) {
  Output_tminus1 <- Output
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
  
  # perform mass balance on stocks
  
  Human_Activity <-
    Human_Activity +
    Human_Activity * Rate_of_Increase_in_Human_Activity - 
    Decrease_in_Human_Activity

  Natural_Capital <-
    Natural_Capital +
    Replenishment_of_Natural_Capital - 
    Depletion_of_Natural_Capital
  
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
  if (Kardashev_Scale >= 3.0) break
  
  # add newest values to output data set
  new_output <- c(year, 
                  Human_Activity,
                  Human_Activity_W,
                  Kardashev_Scale,
                  Natural_Capital,
                  Natural_Capital_NCU,
                  Replenishment_of_Natural_Capital,
                  Unit_Depletion_of_Natural_Capital,
                  Depletion_of_Natural_Capital,
                  Natural_Capital_Gap,
                  Decrease_in_Human_Activity)
  
  names(new_output) <- c("Year", 
                         "Human_Activity",
                         "Human_Activity_W",
                         "Kardashev_Scale",
                         "Natural_Capital",
                         "Natural_Capital_NCU",
                         "Replenishment_of_Natural_Capital",
                         "Unit_Depletion_of_Natural_Capital",
                         "Depletion_of_Natural_Capital",
                         "Natural_Capital_Gap",
                         "Decrease_in_Human_Activity")

  Output <- rbind(Output, new_output)

}
###################################################################################

#Output # uncomment to print output data set if desired

# plot outputs of interest

# # plot human activity stock
# plot(
#   Output[,3]~Output[,1],
#   type="l",
#   xlab="Year",
#   ylab="Human Activity (Kardashev Scale)",
#   log="x"
#   )
# 
# # plot natural capital stock
# plot(
#   Output[,1], Output[,5],
#   log="x",
#   type="l",
#   xlab="Year",
#   ylab="Natural Capital (arbitrary units, 2013=100)"
#   )

# plot outputs in ggplot2

# convert from a matrix to a data frame (required for ggplot)
Output_df <- as.data.frame(Output)

# PLOT HUMAN ACTIVITY (FIGURE 13)
# assign data to be plotted
Human_Activity_Plot_1 <- ggplot(
  data = Output_df, # data set
  aes(x = Output_df$Year, # x variable
      y = Output_df$Human_Activity_W # y variable
  ),
)

# assign type of plot and any statistics to calculate
Human_Activity_Plot_2 <- Human_Activity_Plot_1 + 
  geom_line( # a basic line
    stat = "identity", # plot the actual value, no statistical transformation
    color = "blue", # set the line color
    size = 1.25 # set the line width
  )

# assign a theme
Human_Activity_Plot_3 <- Human_Activity_Plot_2 +
  theme_bw()  # a basic black and white theme

# log transform axes
Human_Activity_Plot_6 <- Human_Activity_Plot_3 +
  coord_trans(
    x = "log10", # transform x axis to log scale 
    y = "log10" # transform y axis to log scale
  )

# set axis labels, scales, and intervals

Human_Activity_Plot_7 <- Human_Activity_Plot_6 +
  scale_x_continuous(
    name = "Years", # x axis label
    limits = c(1, 10000), # set x axis limits
    breaks = c(5, 10, 50, 100, 500, 1000, 10000), # x axis limits and interval
    minor_breaks = function(x) log10(x)/10, # specify minor breaks (NOT DEBUGGED)
    expand = c(0.01,0) # control where y axis crosses - first number is fraction of plot left as white space
    #     labels = c("Years", "", "Decades", "", "Centuries", "") # replace x-axis numbers with text labels
  ) +
  scale_y_continuous(
    name = ("Human Activity (W)"), # y axis label
    limits = c(1e13, 1e36), # set y axis limits
    breaks = c(1e13, 1e15, 1e17, 1e19, 1e21, 
               1e23, 1e25, 1e27, 1e29, 1e31,
               1e33, 1e35), # set breaks and tick marks
    expand = c(0.02,0) # control where x axis crosses - first number is fraction left as white space
  )

# set font size of labels and numbers on axes

Human_Activity_Plot_8 <- Human_Activity_Plot_7 +
  theme(
    axis.title.x = element_text(size = 18), # set font size of x axis label
    axis.text.x = element_text(size = 15), # set font size of x axis text
    axis.title.y = element_text(size = 18), # set font size of y axis label
    axis.text.y = element_text(size = 15), # set font size of y axis text
    panel.grid.minor = element_line() # specify minor grid lines (NOT DEBUGGED)
  )

# set tick marks

Human_Activity_Plot_9 <- Human_Activity_Plot_8 +
  annotation_logticks( # adds tick marks to a log scale
    scaled = FALSE # tells R we have transformed the axis, not the data
  )

Human_Activity_Plot_9


# PLOT NATURAL CAPITAL

# assign data to be plotted
Natural_Capital_Plot_1 <- ggplot(
  data = Output_df, # data set
  aes(x = Output_df$Year, # x variable
      y = Output_df$Natural_Capital # y variable
  ),
)

# assign type of plot and any statistics to calculate
Natural_Capital_Plot_2 <- Natural_Capital_Plot_1 + 
  geom_line( # a basic line
    stat = "identity", # plot the actual value, no statistical transformation
    color = "blue", # set the line color
    size = 1.25 # set the line width
  )

# assign a theme
Natural_Capital_Plot_3 <- Natural_Capital_Plot_2 +
  theme_bw()  # a basic black and white theme

# log transform axes
Natural_Capital_Plot_4 <- Natural_Capital_Plot_3 +
  coord_trans(
    x = "log10", # transform x axis to log scale 
    y = "log10" # transform y axis to log scale
  )

# set axis labels, scales, and intervals

Natural_Capital_Plot_5 <- Natural_Capital_Plot_4 +
  scale_x_continuous(
    name = "Years", # x axis label
    limits = c(1, 1000), # set x axis limits
    breaks = c(5, 10, 50, 100, 500, 1000), # x axis limits and interval
    minor_breaks = function(x) log10(x)/10, # specify minor breaks (NOT DEBUGGED)
    expand = c(0.01,0) # control where y axis crosses - first number is fraction of plot left as white space
    #     labels = c("Years", "", "Decades", "", "Centuries", "") # replace x-axis numbers with text labels
  ) +
  scale_y_continuous(
    name = ("Natural Capital (kg C)"), # y axis label
    limits = c(1.3e15, 1.8e15), # set y axis limits
    breaks = c(1.3e15, 1.4e15, 1.5e15, 1.6e15, 1.7e15, 1.8e15), # set breaks and tick marks
    expand = c(0.02,0) # control where x axis crosses - first number is fraction left as white space
  )

# set font size of labels and numbers on axes

Natural_Capital_Plot_6 <- Natural_Capital_Plot_5 +
  theme(
    axis.title.x = element_text(size = 18), # set font size of x axis label
    axis.text.x = element_text(size = 15), # set font size of x axis text
    axis.title.y = element_text(size = 18), # set font size of y axis label
    axis.text.y = element_text(size = 15), # set font size of y axis text
    panel.grid.minor = element_line() # specify minor grid lines (NOT DEBUGGED)
  )

# set tick marks

Natural_Capital_Plot_7 <- Natural_Capital_Plot_6 +
  annotation_logticks( # adds tick marks to a log scale
    scaled = FALSE # tells R we have transformed the axis, not the data
  )

Natural_Capital_Plot_7
