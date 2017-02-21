#========================================================================================#
# R script to plot growth.txt output file from TTR model
#----------------------------------------------------------------------------------------#

# To do list:
# - Add utilisation

# Add the function add.alpha and set alpha value
#----------------------------------------------------------------------------------------#
source ('setup.R')

# Plot PLant C
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_growth.png')

  # Plot graph 
  m.plot (var            = growth, 
          offset         = 2,
          n_compartments = 10,
          ylab           = 'Total plant growth [kg dm]',
          single.axis    = FALSE)
  
  # Add leaf growth
  s.lines (x      = growth [, 1], 
           y1     = rowSums (growth [, c (3:12)]),
           y2     = rowSums (growth [, c (4:7, 9:12)]),
           colour = c.c["Leaves"])	

  # Add branch growth
  s.lines (x      = growth [, 1], 
           y1     = rowSums (growth [, c (4:7, 9:12)]),
           y2     = rowSums (growth [, c (5:7, 10:12)]),
           colour = c.c["Branches"])
 
  # Add stem growth
  s.lines (x      = growth [, 1], 
           y1     = rowSums (growth [, c (5:7, 10:12)]),
           y2     = rowSums (growth [, c (6, 7, 11, 12)]),
           colour = c.c["Stems"])

  # Add coarse root growth
  s.lines (x      = growth [, 1], 
           y1     = rowSums (growth [, c (6, 7, 11, 12)]),
           y2     = rowSums (growth [, c (7, 12)]),
           colour = c.c["Coarse roots"])
  
  # Add fine root growth
  s.lines (x      = growth [, 1], 
           y1     = rowSums (growth [, c (7, 12)]),
           y2     = rep (0, length (growth [, 1])),
           colour = c.c["Fine roots and mycorrhiza"])
  
  # Add leaf area growth
  par (new = TRUE)
  plot (x    = growth [, 1],
        y    = growth [, 2],
        typ  = 'l',
        lwd  = 2, 
        lty  = 2,
        xlab = xlab_time,
        xaxt = 'n', yaxt = 'n',
        ylab = '',
        col  = add.alpha ('#91b9a4', alpha))
  axis (4)
  mtext (expression (paste ('Leaf area growth [',m^2,']')), side = 4, line = 3)

  # Legend  
  m.legend (ytot    = growth [, 2], 
            items   = c ('Leaf area','Leaves', 'Branches', 'Stems', 'Coarse roots', 'Fine roots and mycorrhiza'), 
            ltys    = c (2, rep (1, 5)),
            colours = c ('#91b9a4',c.c))
dev.off ()


# Plot plant structural growth
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_X_growth.png')

  # Plot graph
  m.plot (var    = growth, 
          offset = 2,
          ylab   = 'Plant structural growth [kg dm]')
  
  # Loop through compartments
  compartments <- l.compartments (var = growth, offset = 2)

  # Add a legend   
  m.legend (ytot = rowSums (growth [, c (3:7)]))
dev.off ()

# Plot plant meristem growth
#----------------------------------------------------------------------------------------#
png (file = 'fig/plant_M_growth.png')

  # Plot graph
  m.plot (var    = growth,
          offset = 7,
          ylab   = 'Plant meristem growth [kg dm]')

  # Loop through compartments
  compartments <- l.compartments (var = growth, offset = 7)

  # Add a legend   
  m.legend (ytot = rowSums (growth [, c (8:12)]))

dev.off ()
#========================================================================================#