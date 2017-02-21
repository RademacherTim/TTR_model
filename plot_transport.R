#========================================================================================#
# R script to plot transport.txt output file from TTR model
#----------------------------------------------------------------------------------------#

# To do list:

# Add the function add.alpha and set alpha value
#----------------------------------------------------------------------------------------#
source ('setup.R')

# Plot pLant C transport
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_C_transport.png')

  # Plot graph 
  m.plot (var            = transp,
          n_compartments = 4,
          ylab           = 'Plant C transport [kg C]',
          l_limit        = min (transp [, 2:5]),
          poly           = FALSE)

  # Loop through compartments
  compartments <- l.compartments (var           = transp, 
                                  n_compartment = 4, 
                                  poly          = FALSE, 
                                  colours       = t.c.c)
  
  # Add a legend
  m.legend (ytot    = max (transp [, c (2:5)]), 
            colours = t.c.c,
            items   = c ('Leaves to branches', 'Branches to stems', 'Stems to coarse roots', 'Coarse to fine roots and mycorrhiza'))
dev.off ()

# Plot pLant N transport
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_N_transport.png')

  # Plot graph 
  m.plot (var            = transp,
          n_compartments = 4,
          offset         = 5,
          ylab           = 'Plant N transport [kg C]',
          l_limit        = min (transp [, 6:9]),
          poly           = FALSE)
  
  # Loop through compartments
  compartments <- l.compartments (var           = transp, 
                                  n_compartment = 4, 
                                  offset        = 5, 
                                  poly          = FALSE, 
                                  colours       = t.n.c)

  # Add a legend   
  m.legend (ytot    = max (transp [, c (6:9)]), 
            colours = t.n.c,
            items   = c ('Branches to leaves', 'Stems to branches', 'Coarse roots to stems', 'Fine roots and mycorrhiza to coarse roots'))
dev.off ()