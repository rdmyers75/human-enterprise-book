# This code converts units, does calculations, and creates graphics for
# the book Human Enterprise, Ecosystems, and the Future of Civilization.

###################################################################################
# load libraries
library(ggplot2)

###################################################################################
# The code below does calculations, conversions, and creates
# Figure 2 in the book.

# Purpose: Estimate kg of C in oil reserves

# constants and assumptions
number_barrels_oil = 1668.9 * 1e3 * 1e6 # 1,668/9 thousand million barrels" of oil
volume_one_barrel_oil <- 159 # L
density_oil <- 0.815 # g/cm^3
fraction_C <- 0.85 # fraction of oil assumed to be carbon

# calculation
C_kg_oil <- # kg of carbon 
  number_barrels_oil * # number of barrels of oil
  volume_one_barrel_oil * # volume of one barrel in L / barrel
  1 / 1000 * # 1 m^3 / 1000 L
  100^3 * # 100 cm^3 / 1 m^3
  density_oil * # density of oil, g/cm^3
  1 /1000 * # kg / 1000 g
  fraction_C # dimensionless fraction of oil that is made up of C

C_kg_oil # print result, C in one barrel of oil, kg

# Purpose: Estimate kg of C in coal reserves

# constants and assumptions
coal_reserves_tonnes <- 860938e6 #860,938 million tonnes
MW_C <- 12.0107 # molecular weight of carbon
MW_H <- 1.00794 # molecular weight of hydrogen
MW_O <- 15.9994 # molecular weight of oxygen
MW_N <- 14.0067 # molecular weigth of nitrogen
MW_S <- 32.065 # molecular weight of sulfur
# Note a random internet sources gives a typical chemical formula for coal
# as C135 H96 O9 N1 S1

# calculation
C_kg_coal <- # estimated kg C in coal reserves
  coal_reserves_tonnes * # coal reserves in tonnes
  1000 * # 1000 kg / 1 tonne
  MW_C*135 / (MW_C*135 + MW_H*96 + MW_O*9 + MW_N*1 + MW_S*1) # estimated fraction C

C_kg_coal # print estimate of kg C in coal reserves

# Purpose: Estimate kg of C in natural gas reserves

# constants and assumptions
nat_gas_reserves_m3 <- 187.3e12 # 187.3 trillion cubic meters natural gas
density_nat_gas <- 22.4 # 22.4 L/mol
# Note: assume natural gas is pure CH4

# calculation
C_kg_nat_gas <- # estimated kg C in natural gas reserves
  nat_gas_reserves_m3 * # natural gas reserves, m^3
  1000 * # 1000 L / 1 m^3
  1/density_nat_gas * # inverse density natural gas, mol/L
  (MW_C + MW_H*4) * # g/mol natural gas
  1 / 1000 # kg / 1000 g

C_kg_nat_gas # print estimate of kg C in natural gas reserves

# Purpose: Convert estimate of total carbon stock in forests to kg C

# constants and assumptions
C_forest_tonnes <- 652371e6 # 652,371 million tons of carbon

# calculation
C_forest_kg <- # estimate of total C in forests, including dead wood and soil, kg
  C_forest_tonnes *
  1000 #1000 kg / tonne

C_forest_kg # print estimate of kg C in forests

# Purpose: Sum the estimates of carbon in fossil fuel reserves and forests

# calculation
natural_capital_kgC <- C_kg_oil + C_kg_coal + C_kg_nat_gas + C_forest_kg

natural_capital_kgC # print the estimate of total natural capital, kg C

# Purpose: Convert estimate of primary productivity from Pg/yr to kg/yr
# and make a graph

# constants and assumptions
NPP_Pgyr <- 53.6 # Pg C/yr according to Running

# calculation
NPP_kgyr <-
  NPP_Pgyr * # net primary productity, Pg/yr
  10^15 * # g/Pg
  1 / 1000 # 1 kg / 1000 g

NPP_kgyr # print estimate of net primary productivity in kg C/yr

### make a stacked bar graph of natural capital and primary productivity

# first, create the data set

natural_capital_df <- # data frame containing data to be plotted
  data.frame(
    label = c("Coal", "Oil", "Natural Gas", "Forests", "NPP"),
    value = c(C_kg_coal, C_kg_oil, C_kg_nat_gas, C_forest_kg, NPP_kgyr),
    category = c(rep("Fossil Fuels (kg C)", 3), "Forests (kg C)", "NPP (kg C/yr)")
  )

# use factor() to set desired plot order
natural_capital_df$category <- factor(natural_capital_df$category, 
                                      levels = c(
                                        "Fossil Fuels (kg C)", 
                                        "Forests (kg C)",
                                        "NPP (kg C/yr)"
                                      )
)

# now, make the graph
C_stacked_bar_1 <- ggplot(
  data=natural_capital_df, 
  aes(
    x=category, 
    y=value,
    fill=label  # creates a stacked bar
  )
) +
  geom_bar(stat="identity") +  
  xlab("") +  # no x label
  ylab("Carbon Content") + # change y label
  theme_bw() +  # plain black and white theme
  scale_fill_discrete(name ="",            # control order of legend
                      breaks=c("Coal", 
                               "Oil", 
                               "Natural Gas", 
                               "Forests", 
                               "NPP")
  ) +
  scale_y_continuous(limits=c(0,1.2e15),   # control the y axis
                     breaks=(seq(0,1.2e15,0.2e15)))

C_stacked_bar_1

C_stacked_bar <- C_stacked_bar_1 +
  theme(
    axis.title.x = element_text(size = 18), # set font size of x axis label
    axis.text.x = element_text(size = 15), # set font size of x axis text
    axis.title.y = element_text(size = 18), # set font size of y axis label
    axis.text.y = element_text(size = 15), # set font size of y axis text
    legend.title = NULL, # element_text(size = 18), # set font size for legend title
    legend.text = element_text(size = 15) # set font size for legend text
  )

# print the graph
C_stacked_bar

###################################################################################

###################################################################################
# The code below does calculations, conversions, and creates
# Figure 4 in the book. Note that this requires an external data set.

# Purpose: read in and plot data on oil production 
# note this reads an external data set - user needs to set the local path
oil_production_tbd <- read.table(  # oil production, thousand barrels per dat
  'E:\\Book\\Graphics\\Github\\human-enterprise-book\\Oil_Data_BP_Production_28Nov2013.csv',
  sep = ",",
  col.names = c("Year","Production_tbd"),
  skip = 2
)

head(oil_production_tbd, 5)

# PLOT OIL PRODUCTION DATA

# initiate plot
oil_production_plot1 <- ggplot(
  data = oil_production_tbd,
  aes(x=Year, y=Production_tbd)
)

# add bars
oil_production_plot2 <- oil_production_plot1 + 
  geom_bar(
    stat="identity",
    fill = "blue" # make the bars blue
  )
oil_production_plot2

oil_production_plot2b <- oil_production_plot2 + theme_bw()
oil_production_plot2b

# set axis labels and fonts
oil_production_plot3 <- oil_production_plot2 +
  theme(
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(size = 15, color = "black"), # set font size and color of x axis text
    axis.text.y = element_text(size = 15, color = "black"), # set font size and color of y axis text
    panel.background =  element_rect(fill = "white", colour = NA), # set white background
    panel.border =      element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  element_line(colour = "grey90", size = 0.2), # set major grid lines
    panel.grid.minor =  element_line(colour = "grey98", size = 0.5) # set minor grid lines
  )

oil_production_plot3

oil_production_plot3b <- oil_production_plot3 + 
  labs(x="Year", y = "Production (thousand barrels per day)")

oil_production_plot3b

# set axis limits and intervals
oil_production_plot4 <- oil_production_plot3b +
  scale_x_continuous(limits=c(1986,2013), breaks=seq(1986,2013,2)) +
  scale_y_continuous(limits=c(0,100000), breaks=(seq(0,100000,10000)))
oil_production_plot4

###################################################################################

###################################################################################
# The code below does calculations, conversions, and creates
# Figure 5 in the book.

# Purpose: read in and plot data on oil prices
# requires an external data set, available on Github

oil_price <- read.table(  # Brent crude price
  'E:\\Book\\Graphics\\Github\\human-enterprise-book\\Oil_Data_USEAI_Price_28Nov2013.csv',
  sep = ",",
  col.names = c("Date1","Price_USD", "Price_2012_USD", "Year", "Price_Mult"),
  skip = 3
)

oil_price$Date1 <- as.Date(oil_price$Date1, "%m/%d/%Y")

head(oil_price,n=10)

# PLOT OIL PRICE DATA
# initiate the plot
oil_price_plot1 <- ggplot(
  data = oil_price,
  aes(x=factor(Year), y=Price_2012_USD)
)

# create box plots where whiskers extend all the way to max and min
oil_price_plot2 <- oil_price_plot1 + geom_boxplot(coef=Inf, color="blue")

# change axis label size and make bold
oil_price_plot3 <- oil_price_plot2 +
  theme(
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(size = 15, color = "black"), # set font size and color of x axis text
    axis.text.y = element_text(size = 15, color = "black"), # set font size and color of y axis text
    panel.background =  element_rect(fill = "white", colour = NA), # set white background
    panel.border =      element_rect(fill = NA, colour="black"), # set black border
    panel.grid.major =  element_line(colour = "grey90", size = 0.2), # set major grid lines
    panel.grid.minor =  element_line(colour = "grey98", size = 0.5) # set minor grid lines
  ) + 
  labs(x="Year", y = "Price (Constant 2012 USD)")

# change y (continuous) axis limits and intervals
oil_price_plot4 <- oil_price_plot3 +
  scale_y_continuous(
    limits=c(0,160), 
    breaks=seq(0,160,20)
  )

# change x (discrete) axis limits and intervals
oil_price_plot5 <- oil_price_plot4 +
  scale_x_discrete(
    breaks=seq(1985, 2015, 5)
  )

# print the plot
oil_price_plot5

###################################################################################

###################################################################################
# The code below does calculations, conversions, and creates
# Figure 10 in the book.

# Purpose: Convert GJ per tonne oil equivalent to J/kg oil equivalent


# Purpose: fit a trend line to energy supply data from Keay (2007)

# constants and assumptions
GJ_per_tonne_oe <- 42 # GJ per tonne oil equivalent

# calculation
J_per_kg <- # J energy in one kg of oil equivalent
  GJ_per_tonne_oe * # GJ / tonne
  1 / 1000 * # 1 tonne / 1000 kg
  1e9 # 1e9 J / GJ

J_per_kg # print result

# also calculate kg C per J of energy used in 2000

kg_C_per_kgoe <- 0.76 # kg C per kg oil eqivalent

kg_C_per_J <- kg_C_per_kgoe / J_per_kg

kg_C_per_J # print result

# create a data frame from Table 3, p. 11

energy_supply_data <- data.frame(
  year = c(1820, 1870, 1913, 1950, 1973, 2003),
  energy_supply_MToe = c(221, 388, 1093, 2130, 6043, 10723)
)

# convert to J

energy_supply_data$energy_supply_J <- # annual energy supply in J
  energy_supply_data$energy_supply_MToe * # energy supply in million metric tonnes of oil equivalent
  1e6 * # tonnes / million tonnes
  GJ_per_tonne_oe * # GJ per tonne oil equivalent
  1e9 # 1e9 J per GJ

energy_supply_data$energy_supply_W <- # annual energy supply in W
  energy_supply_data$energy_supply_J * # energy supply, J/yr
  1 / 365 * # 1 yr / 365 days
  1 / 24 * # 1 day / 24 hrs
  1 / 60 * # 1 hr / 60 min
  1 / 60 # 1 min / 60 s

energy_supply_data # print the data frame

# # plot the data
# plot(energy_supply_data$energy_supply_W~energy_supply_data$year,
#      xlab="Year",
#      ylab="Energy Supply (W)",
#      xlim=c(1800,2000),
# )

# lognormal curve fit

energy_supply_predicted_W <- glm(energy_supply_W~year, 
                                 data=energy_supply_data, family=gaussian(link="log"))

energy_supply_predicted_W  # print the regression results

energy_supply_data$energy_supply_predicted_trend_W <- 
  exp(-19.27185 + 0.02475*energy_supply_data$year)

energy_supply_data # print the data frame


# plot the predicted relationship overlaid on the data

# lines(
#   y=exp(-19.27185+0.02475*energy_supply_data$year), 
#   x=energy_supply_data$year,
#   )

# plot using ggplot2

# assign data to be plotted
Energy_Supply_Plot1 <- ggplot(
  data = energy_supply_data, # data set
  aes(x = year, # x variable
      y = energy_supply_W  # y variable
  )
)

# assign type of plot and any statistics to calculate

Energy_Supply_Plot_2 <- Energy_Supply_Plot1 + 
  geom_point(color = "blue", size = 6, shape = 1)

Energy_Supply_Plot_2

# add an exponential trend line

Energy_Supply_Plot_3 <- Energy_Supply_Plot_2 + 
  geom_smooth(method="glm",family=gaussian(link="log"), se=FALSE,
              color = "blue", size = 1.25)


Energy_Supply_Plot_3


# assign a theme
Energy_Supply_Plot_4 <- Energy_Supply_Plot_3 +
  theme_bw()  # a basic black and white theme


# set axis labels, scales, and intervals

Energy_Supply_Plot_5 <- Energy_Supply_Plot_4 +
  scale_x_continuous(
    name = "Year", # x axis label
    limits = c(1820, 2020), # set x axis limits
    breaks = seq(1820, 2020, 20), # x axis limits and interval
    #     minor_breaks = function(x) log10(x)/10, # specify minor breaks (NOT DEBUGGED)
    expand = c(0.01,0) # control where y axis crosses - first number is fraction of plot left as white space
    #     labels = c("Years", "", "Decades", "", "Centuries", "") # replace x-axis numbers with text labels
  ) +
  scale_y_continuous(
    name = ("Energy Supply (W)"), # y axis label
    limits = c(0, 1.6e13), # set y axis limits
    breaks = seq(0, 1.6e13, 2e12), # set breaks and tick marks
    expand = c(0.02,0) # control where x axis crosses - first number is fraction left as white space
  )

# set font size of labels and numbers on axes

Energy_Supply_Plot_6 <- Energy_Supply_Plot_5 +
  theme(
    axis.title.x = element_text(size = 18), # set font size of x axis label
    axis.text.x = element_text(size = 15), # set font size of x axis text
    axis.title.y = element_text(size = 18), # set font size of y axis label
    axis.text.y = element_text(size = 15), # set font size of y axis text
    panel.grid.minor = element_line() # specify minor grid lines
  )

Energy_Supply_Plot_6

# set tick marks

# calculate expected value in 2013
energy_use_2013 <- exp(-19.27185 + 0.02475*2013)

energy_use_2013 # print result

###################################################################################
# Purpose: Convert feedback relationship from 10% / 1000 Gt to fraction/kg

# constants and assumptions
reduction_per1000Gt <- 0.165 # ~16.5% per 1000 Gt C

# calculation
reduction_perkg <- # dimensionless reduction in activity per kg C gap
  reduction_per1000Gt * # fraction / 1000 Gt
  1 / 1000e9 * # 1000 Gt / 1000e9 tonnes
  1 / 1000 # 1 tonne / 1000 kg

reduction_perkg # print result

###################################################################################
# Purpose: estimate natural capital depletion per unit of energy used

energy_consumption_2012_J <- # 2012 energy consumption in J
  12476.6 * # million tonnes oil equivalent
  1e6 * # tonnes / millon tonnes
  42 * # GJ / tonne oil equivalent
  1e9 # J/GJ

# result: 5.24e20 J (matches supply estimate very well)

# fit curve to fossil fuel portion of energy supply
# fossil fuel portion according to Keay ~ 89.6% of total
fossil_fuel_fraction <- 9579 / (9579 + 1114)
fossil_fuel_portion_J <- energy_consumption_2012_J * fossil_fuel_fraction

# estimate new natural capital depletion as primary productivity 

C_from_fossil_fuels_2012_kg <- # 8.49e12 kg C from fossil fuel use
  fossil_fuel_portion_J *
  kg_C_per_J

# appropriation plus fossil fuel carbon emissions
# human appropriation of primary productivity
human_appropriation_of_NPP_kgC <- # 2.0368e13 kg C / yr
  NPP_kgyr * 0.38

# estimate new natural capital depletion per J of energy used

new_estimate_natural_capital_depletion_kgCperyr <- # 2.89e13 kg C / yr
  human_appropriation_of_NPP_kgC +
  C_from_fossil_fuels_2012_kg

new_estimate_nat_cap_depl_kgCperJperyr <- # 5.5079e-8
  new_estimate_natural_capital_depletion_kgCperyr /
  energy_consumption_2012_J

###################################################################################
# Purpose: data from Costanza, 1997 and IMF, 1999

Costanza_data <- data.frame(
  service =           c("Gas_regulation",
                        "Climate_regulation",
                        "Disturbance_regulation",
                        "Water_regulation",
                        "Water_supply",
                        "Erosion_control",
                        "Soil_formation",
                        "Nutrient_cycling",
                        "Waste_treatment",
                        "Pollination",
                        "Biological_control",
                        "Habitat_refugia",
                        "Food_production",
                        "Raw_materials",
                        "Genetic_resources",
                        "Recreation",
                        "Cultural",
                        "GDP"),
  value_1997USDe9 = c(1341,
                      684,
                      1779,
                      1115,
                      1692,
                      576,
                      53,
                      17075,
                      2277,
                      117,
                      417,
                      124,
                      1386,
                      721,
                      79,
                      815,
                      3015,
                      29477.15),
  category = c(rep("ecosystem_service", 17), "GDP")
)

###################################################################################
# Purpose: data and wild assumptions on probability of disaster

# this is the database of war, plague, and disaster probabilities
#   event (character string): name of the event
#   probability (fraction): total probability of event, for entire duration
#   duration_frac (fraction): fraction of the time this risk is active
#   percent_killed (%): estimated percentage of world population killed by 
#     this event
#   Note: "-99999" is a placeholder number
#   Assumptions:
#     - something like the Cold War will happen about once a century, and
#       last about 40 years
#     - assume estimated species extinction rate from Chicxulub impact 65
#       million years ago, and Permian extinction 250 million years ago, is
#       a good guide to fraction of human beings that would be killed
#     - a dumb physics experiment, such as a miniature black hole or 
#       nanobots capable of self-replication, will be run for about 10 years
#       each century; result of a mishap would be to destroy all life
disaster_database <- data.frame(
  event = c("global_nuclear_war", 
            "asteroid_10km", 
            "physics_experiment_amok"),
  probability = c(1/6, 
                  1/75e6, 
                  1/50e6),
  duration_frac = c(40/100, 
                    1, 
                    10/100),
  percent_killed = c(-99999, 
                     -99999, 
                     100),
  reference = c("Rees, Loc 406", "Rees, Loc 1264", "Rees, Loc 1709")
  )

###################################################################################
# Purpose: convert erg/s to W

Kardashev_I_W <- 4e19 * # erg/s
    1e-7 # W / erg/s

Kardashev_II_W <- 4e33 * # erg/s
  1e-7 # W / erg/s

Kardashev_III_W <- 4e44 * # erg/s
  1e-7 # W / erg/s

Kardashev_I_W
Kardashev_II_W
Kardashev_III_W
