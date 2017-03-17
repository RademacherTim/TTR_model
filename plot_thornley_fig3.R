#=======================================================================================#
# Plot figure 3 from Thornley (1991)
#---------------------------------------------------------------------------------------#

# Read the strucural and meristem pools
#---------------------------------------------------------------------------------------#
pools <- read.table (file = 'tmp/pools.txt', header = T)

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
layout (matrix (c (1, 2)), heights = c (1, 1.25))
par (mar = c (1, 6, 1, 1))
plot (x    = pools [, 1],
      y    = pools [, 16],
      typ  = 'l',
      ylim = c (0, 16),
      xaxt = 'n',
      yaxt = 'n',
      ylab = expression (atop (M[lX]*', '*M[bX]*', '*M[cX]*', '*M[fX]*', '*M[sX]*'/20',' (kg dm '*stem^-1*')')),
      col  = c.c [1])
axis (2, at = seq (0,16, by = 2))
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label =  NA)
text (0, 16, '(A)', pos = 1)
lines (x = pools [, 1],
       y = pools [, 17],
       col  = c.c [2])
lines (x = pools [, 1],
       y = pools [, 18] / 20.0,
       col  = c.c [3])
lines (x = pools [, 1],
       y = pools [, 19],
       lty = 2,
       col  = c.c [4])
lines (x = pools [, 1],
       y = pools [, 20],
       lty = 2,
       col  = c.c [5])

par (mar = c (5, 6, 1, 1))
plot (x    = pools [, 1],
      y    = pools [, 11],
      typ  = 'l',
      ylim = c (0, 0.08),
      xaxt = 'n',
      xlab = 'Time (years)',
      ylab = expression (atop (M[lM]*', '*M[bM]*', '*M[cM]*', '*M[fM]*', '*M[sM]*'/4',' (kg dm '*stem^-1*')')),
      col  = c.c [1])
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label = labels [seq (1, length (labels), 10)])
text (0, 0.08, '(B)', pos = 1)
lines (x = pools [, 1],
       y = pools [, 12],
       col  = c.c [2])
lines (x = pools [, 1],
       y = pools [, 13] / 4.0,
       col  = c.c [3])
lines (x = pools [, 1],
       y = pools [, 14],
       lty = 2,
       col  = c.c [4])
lines (x = pools [, 1],
       y = pools [, 15],
       lty = 2,
       col  = c.c [5])
#=======================================================================================#

