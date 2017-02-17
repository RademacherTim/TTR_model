#========================================================================================#
# R script to plot loss.txt and respiration.txt output file from TTR model
#----------------------------------------------------------------------------------------#

# To do list:
# - Separate growth and maintenance respiration
# - Separate losses due to differentiation and due to litter
# - Create graph of total losses including loss, respiration and utilisation?!?

# Add the function add.alpha and set alpha value
#----------------------------------------------------------------------------------------#
source ('setup.R')

# Read in the pool data
#----------------------------------------------------------------------------------------#
loss <- read.table (file = 'tmp/loss.txt',        header = T)
resp <- read.table (file = 'tmp/respiration.txt', header = T)
util <- read.table (file = 'tmp/utilisation.txt', header = T)

# Plot pLant drymass loss
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_loss.png')

  # Plot graph
  m.plot (var    = loss, 
          ylab   = 'Total plant loss [kg dm]',
          n_compartments = 10)
  
  # Loop through compartments
  compartments <- l.compartments (var = loss, n_compartments = 10)
  
  # Add a legend   
  m.legend (ytot = rowSums (loss [, c (2:11)]))
dev.off ()


# Plot total pLant respiration
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_total_respiration.png')

  # Plot graph
  m.plot (var    = resp, 
          ylab   = 'Total plant respiration [kg C]',
          n_compartments = 10)

  # Loop through compartments
  compartments <- l.compartments (var = resp, n_compartments = 10)

  # Add a legend   
  m.legend (ytot = rowSums (resp [, c (2:11)]))
dev.off ()

# Plot C utilisation by the plant
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_C_utilisation.png')

  # Plot graph
  m.plot (var    = util, 
          ylab   = 'Total plant C utilisationn [kg C]')

  # Loop through compartments
  compartments <- l.compartments (var = util)

  # Add a legend   
  m.legend (ytot = rowSums (util [, c (2:11)]))
dev.off ()

# Plot N utilisation by the plant
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_N_utilisation.png')

# Plot graph
m.plot (var    = util, 
        ylab   = 'Total plant N utilisationn [kg C]')

# Loop through compartments
compartments <- l.compartments (var = util)

# Add a legend   
m.legend (ytot = rowSums (util [, c (2:11)]))
dev.off ()

#========================================================================================#