#=======================================================================================#
# Plot figure 6 from Thornley (1991)
#---------------------------------------------------------------------------------------#

# Read the strucural and meristem pools
#---------------------------------------------------------------------------------------#
uptake <- read.table (file = 'tmp/uptake.txt',      header = T)
loss   <- read.table (file = 'tmp/loss.txt',        header = T)
resp   <- read.table (file = 'tmp/respiration.txt', header = T)

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

# Plot figure 6
#----------------------------------------------------------------------------------------#
layout (matrix (1))
par (mar = c (5, 6, 1, 1))
plot (x    = resp [, 1],
      y    = rowSums (resp [, 2:11]) * 100.0,
      typ  = 'l',
      ylim = c (0, 4.0),
      xaxt = 'n',
      xlab = 'Time (years)',
      ylab = expression (paste ('Flux of carbon (',10^-2,' kg C ',stem^-1,d^-1,')')),
      col  = '#af95a3')
axis (side  = 1, 
      at    = ats [seq (1, length (ats), 10)], 
      label = labels [seq (1, length (labels), 10)])
lines (x    = uptake [, 1],
       y    = uptake [, 2] * 100.0,
       col  = "#aab300")
lines (x    = loss [, 1],
       y    = rowSums (loss [, c (3, 5, 7, 9, 11)]) * 1000.0,
       col  = "#eb99a9")
legend (x = 0,
        y = 4,
        legend = c ('respirations','photosynthesis','litter'),
        col = c ('#af95a3',"#aab300","#eb99a9"),
        box.lty = 0,
        lwd = 1)
#=======================================================================================#

