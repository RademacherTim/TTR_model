#========================================================================================#
# R script to plot uptake.txt output file from TTR model
#----------------------------------------------------------------------------------------#

# To do list:
# - 

# Add the function add.alpha and set alpha value
#----------------------------------------------------------------------------------------#
source ('setup.R')

# Plot pool data
#----------------------------------------------------------------------------------------#
png (file = 'fig/uptake.png')

  par (mar = c (5, 5, 1, 5))
  plot (x    = uptake [, 1],
        y    = uptake [, 2],
        xlab = xlab_time,
        ylab = 'C assimilation [kg C per day]',
        xaxt = 'n',
        typ  = 'l',
        lwd  = 2,
        col  = '#91b9a4')
  axis (1, at = ats, label = labels)
  par (new = TRUE)
  plot (x = uptake [, 1],
        y = uptake [, 3],
        ylab = '',
        xlab = '',
        xaxt = 'n',
        yaxt = 'n',
        typ  = 'l',
        lty  = 2,
        lwd  = 2,
        col  = '#8F2BBC')
  mtext ('N uptake [kg N per day]', side = 4, line = 3)
  axis (4)
dev.off ()

#========================================================================================#