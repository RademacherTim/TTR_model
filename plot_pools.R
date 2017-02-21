#========================================================================================#
# R script to plot pools.txt output file from TTR model
#----------------------------------------------------------------------------------------#

# To do list:
# - Add hatching for shoot and root in polygons to graphs

# Add the function add.alpha and set alpha value
#----------------------------------------------------------------------------------------#
source ('setup.R')

# Plot PLant C
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_C_pools.png')
  
  # Plot graph 
  m.plot (var            = pools, 
          n_compartments = 5,
          ylab           = 'Plant carbon [kg C]')

  # Loop through compartments
  compartments <- l.compartments (var = pools)
  
  # Add a legend   
  m.legend (ytot = rowSums (pools [, c (2:6)]))
dev.off ()


# Plot plant N
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_N_pools.png')

  # Plot graph 
  m.plot (var            = pools, 
          offset         = 6,
          ylab           = 'Plant nitrogen [kg N]')

  # Loop through compartments
  compartments <- l.compartments (var    = pools,
                                  offset = 6)

  # Add a legend   
  m.legend (ytot = rowSums (pools [, c (7:11)]))
dev.off ()

# Plot M
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_M_pools.png')

  # Plot graph 
  m.plot (var            = pools, 
          offset         = 11,
          ylab           = 'Plant meristematic drymass [kg dm]')

  # Loop through compartments
  compartments <- l.compartments (var    = pools,
                                  offset = 11)

  # Add a legend   
  m.legend (ytot = rowSums (pools [, c (12:16)]))

  # Set plot margins
  par (mar = c (5, 5, 1, 1))
dev.off ()

# Plot X
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_X_pools.png')

  # Plot graph 
  m.plot (var            = pools, 
          offset         = 16,
          ylab           = 'Plant structural drymass [kg dm]')

  # Loop through compartments
  compartments <- l.compartments (var    = pools,
                                  offset = 16)

  # Add a legend   
  m.legend (ytot = rowSums (pools [, c (17:21)]))
dev.off ()
#========================================================================================#