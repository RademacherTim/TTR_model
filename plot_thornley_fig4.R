#=======================================================================================#
# Plot figure 4 from Thornley (1991)
#---------------------------------------------------------------------------------------#

# Read the strucural and meristem pools
#---------------------------------------------------------------------------------------#
cons <- read.table (file = 'tmp/concentrations.txt', header = T)

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
c.c         <- c ('#55a51c', '#eee29f', '#faae53', '#a41034', '#af95a3')
names (c.c) <- c ('Leaves',  'Branches','Stems',   'Coarse roots',  'Fine roots and mycorrhiza')

# Plot figure 3
#----------------------------------------------------------------------------------------#
layout (matrix (c (1, 2,3)), heights = c (1.0, 1.0, 1.2))
par (mar = c (1, 6, 1, 1))
plot (x    = cons [, 1],
      y    = cons [, 2],
      typ  = 'l',
      ylim = c (0, 0.2),
      xaxt = 'n',
      xlab = 'Time (years)',
      ylab = expression (atop ('Carbon substrate concentrations',' [kg substrate C (kg dm)'^-1*']')),
      col  = c.c [1])
text (0, 0.2, '(A)', pos = 1)
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label =  NA)
lines (x = cons [, 1],
       y = cons [, 3],
       col  = c.c [2])
lines (x = cons [, 1],
       y = cons [, 4],
       col  = c.c [3])
lines (x = cons [, 1],
       y = cons [, 5],
       lty = 2,
       col  = c.c [4])
lines (x = cons [, 1],
       y = cons [, 6],
       lty = 2,
       col  = c.c [5])

plot (x    = cons [, 1],
      y    = cons [, 7],
      typ  = 'l',
      ylim = c (0, 0.05),
      xaxt = 'n',
      xlab = 'Time (years)',
      ylab = expression (atop ('Nitrogen substrate concentrations',' [kg substrate N (kg dm)'^-1*']')),
      col  = c.c [1])
text (0, 0.05, '(B)', pos = 1)
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label =  NA)
lines (x = cons [, 1],
       y = cons [, 8],
       col  = c.c [2])
lines (x = cons [, 1],
       y = cons [, 9],
       col  = c.c [3])
lines (x = cons [, 1],
       y = cons [, 10],
       lty = 2,
       col  = c.c [4])
lines (x = cons [, 1],
       y = cons [, 11],
       lty = 2,
       col  = c.c [5])

par (mar = c (5, 6, 1, 1))
plot (x    = cons [, 1],
      y    = cons [, 2] * cons [, 7] / 0.001,
      typ  = 'l',
      ylim = c (0, 12.0),
      xaxt = 'n',
      xlab = 'Time (years)',
      ylab = expression (atop ('Product of carbon and nitrogen substrate concentrations, '*C[i]*N[i],' ('*10^-3*' [C] [N])')),
      col  = c.c [1])
text (0, 12.0, '(C)', pos = 1)
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label = labels [seq (1, length (labels), 10)])
lines (x = cons [, 1],
       y =  cons [, 3] * cons [, 8] / 0.001,
       col  = c.c [2])
lines (x = cons [, 1],
       y =  cons [, 4] * cons [, 9] / 0.001,
       col  = c.c [3])
lines (x = cons [, 1],
       y =  cons [, 5] * cons [, 10] / 0.001,
       lty = 2,
       col  = c.c [4])
lines (x = cons [, 1],
       y =  cons [, 6] * cons [, 11] / 0.001,
       lty = 2,
       col  = c.c [5])
#=======================================================================================#

