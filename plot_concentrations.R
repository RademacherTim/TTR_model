#========================================================================================#
# R script to plot concentrations.txt output file from TTR model
#----------------------------------------------------------------------------------------#

# Add the function add.alpha and set alpha value
#----------------------------------------------------------------------------------------#
source ('setup.R')

# Plot PLant C concentrations
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_C_concentrations.png')

  # Plot graph 
  m.plot (var            = concentrations, 
          offset         = 1,
          n_compartments = 5,
          ylab           = expression (paste ('C concentrations ([kg C] [kg dm',']'^-1,')')),
          poly = FALSE)

  # Loop through compartments
  compartments <- l.compartments (var  = concentrations, 
                                  poly = FALSE)

  # Add a legend   
  m.legend (ytot = max (concentrations [, c (2:6)]))
dev.off ()

# Plot PLant N concentrations
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_N_concentrations.png')

  # Plot graph 
  m.plot (var            = concentrations, 
          offset         = 6,
          n_compartments = 5,
          ylab           = expression (paste ('N concentrations ([kg N] [kg dm',']'^-1,')')),
          poly           = FALSE)

  # Loop through compartments
  compartments <- l.compartments (var    = concentrations,
                                  offset = 6, 
                                  poly   = FALSE)

  # Add a legend   
  m.legend (ytot = max (concentrations [, c (7:11)]))
dev.off ()

#========================================================================================#