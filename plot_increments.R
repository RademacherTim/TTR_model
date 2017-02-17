#========================================================================================#
# R script to plot increments.txt output file from TTR model
#----------------------------------------------------------------------------------------#

# To do list:
# - 

# Add the function add.alpha and set alpha value
#----------------------------------------------------------------------------------------#
source ('setup.R')

# Read in the pool data
#----------------------------------------------------------------------------------------#
incs <- read.table (file = 'tmp/increments.txt', header = T)

# Plot leaf increments and leaf area increment
#----------------------------------------------------------------------------------------#
png (file = 'fig/leaf_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 2,
          n_compartments = 4,
          ylab           = 'Leaf increment [kg]',
          single.axis    = FALSE)

  # Loop through compartments
  compartments <- l.compartments (var           = incs, 
                                  n_compartment = 4, 
                                  offset        = 2, 
                                  poly          = FALSE,
                                  colours       = type)
  
  # Add leaf area growth
  par (new = TRUE)
  plot (x    = incs [, 1],
        y    = incs [, 2],
        typ  = 'l',
        lwd  = 2, 
        lty  = 2,
        xlab = xlab_time,
        xaxt = 'n', yaxt = 'n',
        ylab = '',
        col  = add.alpha ('#91b9a4', alpha))
  axis (4)
  mtext (expression (paste ('Leaf area [',m^2,']')), side = 4, line = 3)

  # Add a legend   
  m.legend (ytot    = incs [, 2],
            items   = c ('Leaf area','Meristem','Structure','Carbon','Nitrogen'),
            ltys    = c (2, rep (1, 5)),
            colours = c ('#91b9a4', type))
dev.off ()


# Plot branch increments
#----------------------------------------------------------------------------------------#
png (file = 'fig/branch_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 6,
          n_compartments = 4,
          ylab           = 'Branch increment [kg]')

  # Loop through compartments
  compartments <- l.compartments (var           = incs,
                                  n_compartment = 4, 
                                  offset        = 6,
                                  poly          = FALSE,
                                  colours       = type)

  # Add a legend   
  m.legend (ytot = rowSums (incs [, c (7:10)]),
            items = c ('Meristem','Structure','Carbon','Nitrogen'),
            colours       = type)
dev.off ()

# Plot stem increments
#----------------------------------------------------------------------------------------#
png (file = 'fig/stem_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 10,
          n_compartments = 4,
          ylab           = 'Stem increment [kg]')

  # Loop through compartments
  compartments <- l.compartments (var           = incs,
                                  n_compartment = 4, 
                                  offset        = 10,
                                  poly          = FALSE,
                                  colours       = type)

  # Add a legend   
  m.legend (ytot = rowSums (incs [, c (11:14)]),
            items = c ('Meristem','Structure','Carbon','Nitrogen'),
            colours       = type)
dev.off ()

# Plot coarse root increments
#----------------------------------------------------------------------------------------#
png (file = 'fig/coarse_root_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 14,
          n_compartments = 4,
          ylab           = 'Coarse root increment [kg]')

  # Loop through compartments
  compartments <- l.compartments (var           = incs,
                                  n_compartment = 4, 
                                  offset        = 14,
                                  poly          = FALSE,
                                  colours       = type)

  # Add a legend   
  m.legend (ytot = rowSums (incs [, c (15:18)]),
            items = c ('Meristem','Structure','Carbon','Nitrogen'),
            colours       = type)
dev.off ()

# Plot fine root increments
#----------------------------------------------------------------------------------------#
png (file = 'fig/fine_root_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 18,
          n_compartments = 4,
          ylab           = 'Fine root increment [kg]')

  # Loop through compartments
  compartments <- l.compartments (var           = incs,
                                  n_compartment = 4, 
                                  offset        = 18,
                                  poly          = FALSE,
                                  colours       = type)

  # Add a legend   
  m.legend (ytot = rowSums (incs [, c (19:22)]),
            items = c ('Meristem','Structure','Carbon','Nitrogen'),
            colours       = type)
dev.off ()

#========================================================================================#
# Plot meristem increment
#----------------------------------------------------------------------------------------#
png (file = 'fig/M_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 2,
          n_compartments = 5,
          ylab           = 'Meristem increment [kg]',
          columns        = c (3, 7, 11, 15, 19))

  # Add leaf meristem increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (3, 7, 11, 15, 19)]),
           y2     = rowSums (incs [, c (7, 11, 15, 19)]),
           colour = c.c["Leaves"])	
  
  # Add branch meristem increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (7, 11, 15, 19)]),
           y2     = rowSums (incs [, c (11, 15, 19)]),
           colour = c.c["Branches"])
  
  # Add stem meristem increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (11, 15, 19)]),
           y2     = rowSums (incs [, c (15, 19)]),
           colour = c.c["Stems"])
  
  # Add coarse root meristem increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (15, 19)]),
           y2     = incs [, 19],
           colour = c.c["Coarse roots"])
  
  # Add fine root meristem increment
  s.lines (x      = incs [, 1], 
           y1     = incs [, 19],
           y2     = rep (0, length (growth [, 1])),
           colour = c.c["Fine roots and mycorrhiza"])

  # Add a legend   
  m.legend (ytot = rowSums (incs [, c (3, 7, 11, 15, 19)]))
dev.off ()

# Plot structure increment
#----------------------------------------------------------------------------------------#
png (file = 'fig/X_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 3,
          n_compartments = 5,
          ylab           = 'Structural increment [kg]',
          columns        = c (4, 8, 12, 16, 20))

  # Add leaf structure increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (4, 8, 12, 16, 20)]),
           y2     = rowSums (incs [, c (8, 12, 16, 20)]),
           colour = c.c["Leaves"])	

  # Add branch structure increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (8, 12, 16, 20)]),
           y2     = rowSums (incs [, c (12, 16, 20)]),
           colour = c.c["Branches"])

  # Add stem structure increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (12, 16, 20)]),
           y2     = rowSums (incs [, c (16, 20)]),
           colour = c.c["Stems"])

  # Add coarse root structure increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (16, 20)]),
           y2     = incs [, 20],
           colour = c.c["Coarse roots"])

  # Add fine root structure increment
  s.lines (x      = incs [, 1], 
           y1     = incs [, 20],
           y2     = rep (0, length (growth [, 1])),
           colour = c.c["Fine roots and mycorrhiza"])

  # Add a legend   
  m.legend (ytot = rowSums (incs [, c (4, 8, 12, 16, 20)]))
dev.off ()

# Plot Carbon growth
#----------------------------------------------------------------------------------------#
png (file = 'fig/C_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 4,
          n_compartments = 5,
          ylab           = 'Carbon increment [kg]',
          columns        = c (5, 9, 13, 17, 21))

  # Add leaf C increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (5, 9, 13, 17, 21)]),
           y2     = rowSums (incs [, c (9, 13, 17, 21)]),
           colour = c.c["Leaves"])	

  # Add branch C increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (9, 13, 17, 21)]),
           y2     = rowSums (incs [, c (13, 17, 21)]),
           colour = c.c["Branches"])

  # Add stem C increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (13, 17, 21)]),
           y2     = rowSums (incs [, c (17, 21)]),
           colour = c.c["Stems"])

  # Add coarse root C increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (17, 21)]),
           y2     = incs [, 21],
           colour = c.c["Coarse roots"])

  # Add fine root C increment
  s.lines (x      = incs [, 1], 
           y1     = incs [, 21],
           y2     = rep (0, length (growth [, 1])),
           colour = c.c["Fine roots and mycorrhiza"])

  # Add a legend   
  m.legend (ytot = rowSums (incs [, c (5, 9, 13, 17, 2)]))
dev.off ()

# Plot Nitrogen increments
#----------------------------------------------------------------------------------------#
png (file = 'fig/N_increments.png')

  # Plot graph 
  m.plot (var            = incs, 
          offset         = 5,
          n_compartments = 5,
          ylab           = 'N increment [kg]',
          columns        = c (6, 10, 14, 18, 22))

  # Add leaf N increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (6, 10, 14, 18, 22)]),
           y2     = rowSums (incs [, c (10, 14, 18, 22)]),
           colour = c.c["Leaves"])	

  # Add branch N increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (10, 14, 18, 22)]),
           y2     = rowSums (incs [, c (14, 18, 22)]),
           colour = c.c["Branches"])

  # Add stem N increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (14, 18, 22)]),
           y2     = rowSums (incs [, c (18, 22)]),
           colour = c.c["Stems"])

  # Add coarse N increment
  s.lines (x      = incs [, 1], 
           y1     = rowSums (incs [, c (18, 22)]),
           y2     = incs [, 22],
           colour = c.c["Coarse roots"])

  # Add fine root N increment
  s.lines (x      = incs [, 1], 
           y1     = incs [, 22],
           y2     = rep (0, length (growth [, 1])),
           colour = c.c["Fine roots and mycorrhiza"])

  # Add a legend   
  m.legend (ytot = rowSums (incs [, c (6, 10, 14, 18, 22)]))
dev.off ()
#========================================================================================#