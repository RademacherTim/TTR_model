#=======================================================================================#
# Plot figure 5 from Thornley (1991)
#---------------------------------------------------------------------------------------#

# Read the strucural and meristem pools
#---------------------------------------------------------------------------------------#
transp <- read.table (file = 'tmp/transport.txt', header = T)

# Determine time axis
#----------------------------------------------------------------------------------------#
years        = as.numeric (read.table ('driver.txt', skip = 5, nrows = 1) [1])
stps_per_day = as.numeric (read.table ('driver.txt', skip = 6, nrows = 1) [1])
#n_stems      = as.numeric (read.table ('driver.txt', skip = 6, nrows = 1) [1])
dt     = 1 / stps_per_day
tstps  = years * 365.25 * stps_per_day
days   = dt * tstps
hours  = dt * tstps * 24.0
labels    = seq (0, ceiling (years), by = 1)
ats       = seq (0, ceiling (years), by = 1) * 365.25 / dt

# Colours for compartment
#----------------------------------------------------------------------------------------#
t.c.c       <- c ('#55a29f', '#eeee53', '#faa034', '#a415a3')
t.n.c       <- c ('#eee51c', '#faa29f', '#a41e53', '#af9034')
names (c.c) <- c ('Leaves',  'Branches','Stems',   'Coarse roots',  'Fine roots and mycorrhiza')

# Plot figure 3
#----------------------------------------------------------------------------------------#
layout (matrix (c (1, 2,3)), heights = c (1.0, 1.0, 1.2))
par (mar = c (1, 6, 1, 1))
plot (x    = transp [, 1],
      y    = transp [, 2],
      typ  = 'l',
      ylim = c (0, 0.032),
      xaxt = 'n',
      xlab = 'Time (years)',
      ylab = expression (atop ('C substrate tranport flux','(kg C '*stem^-1*d^-1*')')),
      col  = t.c.c [1])
text (0, 0.032, '(A)', pos = 1)
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label =  NA)
lines (x = transp [, 1],
       y = transp [, 3],
       col  = t.c.c [2])
lines (x = transp [, 1],
       y = transp [, 4],
       col  = t.c.c [3])
lines (x = transp [, 1],
       y = transp [, 5],
       col  = t.c.c [4])

plot (x    = transp [, 1],
      y    = transp [, 6] / 0.001,
      typ  = 'l',
      ylim = c (0, 0.8),
      xaxt = 'n',
      xlab = 'Time (years)',
      ylab = expression (atop ('N substrate transport flux','('*10^-3*' kg N '*stem^-1*d^-1*')')),
      col  = t.n.c [1])
text (x      = 0, 
      y      = 0.8, 
      labels = '(B)', 
      pos    = 1)
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label =  NA)
lines (x = transp [, 1],
       y = transp [, 7] / 0.001,
       col  = t.n.c [2])
lines (x = transp [, 1],
       y = transp [, 8] / 0.001,
       col  = t.n.c [3])
lines (x = transp [, 1],
       y = transp [, 9] / 0.001,
       col  = t.n.c [4])

par (mar = c (5, 6, 1, 1))
plot (x    = transp [, 1],
      y    = transp [, 10],
      typ  = 'l',
      ylim = c (0, 2.5),
      xaxt = 'n',
      xlab = 'Time (years)',
      ylab = expression (atop ('Carbon substrate transport conductances','( kg structural dm'*stem^-1*d^-1*')')),
      col  = t.c.c [1])
text (x      = 0, 
      y      = 2.5, 
      labels = '(C)', 
      pos    = 1)
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label = labels [seq (1, length (labels), 10)])
lines (x   = transp [, 1],
       y   =  transp [, 11],
       col = t.c.c [2])
lines (x   = transp [, 1],
       y   =  transp [, 12],
       col = t.c.c [3])
lines (x   = transp [, 1],
       y   =  transp [, 13],
       col = t.c.c [4])
#=======================================================================================#

