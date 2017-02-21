#========================================================================================#

## Add an alpha value to a colour
#----------------------------------------------------------------------------------------#
add.alpha <- function (col, alpha=1){
  if (missing(col))
    stop ("please provide a vector of colours.")
  apply (sapply(col, col2rgb)/255, 2, 
         function(x)
           rgb(x[1], x[2], x[3], alpha=alpha))
}

# Set alpha value for lines and polygon areas (alpha_A)
#----------------------------------------------------------------------------------------#
alpha   = 0.8
alpha_A = 0.2

# Colours for compartment
#----------------------------------------------------------------------------------------#
c.c         <- c ('#55a51c', '#eee29f', '#faae53', '#a41034', '#af95a3')
t.c.c       <- c ('#55a29f', '#eeee53', '#faa034', '#a415a3')
t.n.c       <- c ('#eee51c', '#faa29f', '#a41e53', '#af9034')
type        <- c ('#55a51c', '#eee29f', '#000000', '#8F2BBC')
names (c.c) <- c ('Leaves',  'Branches','Stems',   'Coarse roots',  'Fine roots and mycorrhiza')

# Read in the concentrations data
#----------------------------------------------------------------------------------------#
pools          <- read.table (file = 'tmp/pools.txt',          header = T)
growth         <- read.table (file = 'tmp/growth.txt',         header = T)
loss           <- read.table (file = 'tmp/loss.txt',           header = T)
util           <- read.table (file = 'tmp/utilisation.txt',    header = T)
resp           <- read.table (file = 'tmp/respiration.txt',    header = T)
transp         <- read.table (file = 'tmp/transport.txt',    header = T)
incs           <- read.table (file = 'tmp/increments.txt',     header = T)
concentrations <- read.table (file = 'tmp/concentrations.txt', header = T)
uptake         <- read.table (file = 'tmp/uptake.txt',     header = T)

# Determine time axis
#----------------------------------------------------------------------------------------#
years        = as.numeric (read.table ('driver.txt', skip = 5, nrows = 1) [1])
stps_per_day = as.numeric (read.table ('driver.txt', skip = 6, nrows = 1) [1])
#n_stems      = as.numeric (read.table ('driver.txt', skip = 6, nrows = 1) [1])
dt     = 1 / stps_per_day
tstps  = years * 365.25 * stps_per_day
days   = dt * tstps
hours  = dt * tstps * 24.0

if (days > 365) {
  xlab_time = 'Time [years]'
  xmax      = ceiling (years)
  labels    = seq (1, xmax, by = 1)
  ats       = seq (1, xmax, by = 1) * 365.25 / dt
  xmax      = xmax * 365.25 / dt
} else if (days > 1) {
  xlab_time = 'Time [days]'
  xmax      = ceiling (days) 
  labels    = seq (1, xmax, by = 1)
  ats       = seq (1, xmax, by = 1) / dt
  xmax      = xmax / dt
} else {
  xlab_time = 'Time [hours]'
  xmax      = ceiling (hours)
  labels    = seq (1, xmax, by = 1)
  ats       = seq (1, xmax, by = 1) / (24.0 * dt)
  xmax      = xmax / (24.0 * dt)
}

# Modified plotting function -> m.plot
#----------------------------------------------------------------------------------------#
m.plot <- function (var, 
                    offset = 1, 
                    ylab, 
                    n_compartments = 5, 
                    single.axis = TRUE, 
                    l_limit = 0,
                    columns = 0,
                    poly    = TRUE) {
  
  # Set plot margins
  if (single.axis) {
    par (mar = c (5, 5, 1, 1))
  } else {
    par (mar = c (5, 5, 1, 5))
  }

  # Determine columns 
  if (columns [1] == 0) {
    i1 = offset + 1
    i2 = offset + n_compartments
    index = c (i1:i2)
  } else {
    index = c (columns)
  }
  y1   = rowSums (var [, index])
  #print (c (i1, i2))
  #print (rowSums (var [, c (i1:i2)]))
  if (poly) {
    ymax = max (y1)  
  } else {
    ymax = max (var [, index])
  }
   
  
  # Plot 
  plot (x = var [, 1],
        y = y1,
        typ = 'l', 
        lty = 0, 
        xlab = xlab_time,
        xaxt = 'n',
        xlim = c (0, xmax),
        ylab = ylab,
        ylim = c (l_limit, ymax))
  axis (1, at = ats, label = labels)
}

# Function to added line with shaded area underneath to produced stacked plots
#----------------------------------------------------------------------------------------#
s.lines <- function (x, y1, y2, colour, poly = TRUE) {
  # Plot line
  lines (x = x, 
         y = y1,
         col = add.alpha (colour, alpha),
         lwd = 2)	
  
  if (poly) {
    # Add colour under the curve
    polygon (x = c (x, rev (x)),
             y = c (y1, rev (y2)),
             col = add.alpha (colour, alpha_A), 
             lty = 0) 
  }
}

# Function to add modified legend 
#----------------------------------------------------------------------------------------#
m.legend <- function (ytot, ltys = 1, colours = c.c,
                      items = c ('Leaves', 'Branches', 'Stems', 'Coarse roots', 'Fine roots and mycorrhiza')) {
  legend (x       = 0, 
          y       = max (ytot), 
          legend  = items,
          box.lty = 0, 
          lwd     = 2,
          lty     = ltys,  
          bg      = 'transparent',
          col     = add.alpha (colours, alpha))
} 

# Function to loop through compartments
#----------------------------------------------------------------------------------------#
l.compartments <- function (var, offset = 1, n_compartments = 5, poly = TRUE, colours = c.c) {
  
  # Loop over the compartments
  sapply (1:n_compartments, function (i) 
    {
      i1 = i + offset
      i2 = i + offset + 1
      i3 = offset + n_compartments
      #print (c (i, i1, i2, i3))
    
      if (poly) {
        # Figure out the appropriate y-coordinates
        y1.1 = if      (i <   n_compartments)      {rowSums (var [, c (i1:i3)])} else {var [, i3]}
        y2.1 = if      (i <  (n_compartments - 1)) {rowSums (var [, c (i2:i3)])} 
               else if (i == (n_compartments - 1)) {var [, i3]}
               else                                {rep (0, length (var [, 1]))}
      } else {
        y1.1 = var [, i1]
        y2.1 = rep (0, length (var [, 1]))
      }
      #print (head (y1.1))
      #print (head (y2.1))
    
      # Plot compartment
      s.lines (x      = var [, 1],
               y1     = y1.1,
               y2     = y2.1,
               colour = colours [i],
               poly   = poly)
      
      return (NULL)
    }
  )
  return (NULL)
}
#========================================================================================#