#========================================================================================#
# TTR Model photosynthesis functions
#----------------------------------------------------------------------------------------#

theta     = 0.85      # Leaf photosynthesis parameter (unitless)
shi_leaf  = 0.03      # Leaf transmission coef        (unitless)
k         = 0.5       # Canopy extinction coef        ([m]2 ground [m]-2 leaf)
tau       = 0.001     # Leaf CO2 conductance          ([m]-1 [s]-1)
N_l_tot_s = 0.040     # Effect of N content on photosynthesis ([kg N] [kg dm]-1) 
alpha_m   = 0.0000010 # Max. leaf photosynthetic efficiency ([kg CO2] [J]-1)
beta      = 0.0000002 # Photorespiration parameter    ([kg CO2] [m]-2 [s]-1)
n_stems   = 0.4       # Stems m-2 ground
A_l       = 10.0       # Leaf surface area
C_CO2     =     400.0     # CO2 concentration               ([ppmv])
J         = 5000000.0     # Daily radiation recept          ([J] [m]-2 [d]-1)
N_amm     =       0.00075 # Soil mineral ammonium level     ([kg N] [m]-2)
N_nit     =       0.00025 # Soil mineral nitrate level      ([kg N] [m]-2)
p_atm     = 101325.0      # Mean daily atmospheric pressure ([Pa])
T_air     =     14.0      # Mean daily air temperature      ([degC])
T_soil    =     14.0      # mean daily soil temperature     ([degC])
h         = 50400         # Day length                      ([s] [d]-1)
C_CO2_air = (C_CO2 / 10e6) * (273.15 / T_air + 273.15) * (p_atm / 101325) * 1.9636
N_l_tot   = 1.0 # 

L       = n_stems * A_l # Calculate plot leaf area index   ([m]2 leaf [m]2 ground)

I       = J / h         # Calculate instantaneous light flux density ([J] [m]-2)

P_max20 = tau * C_CO2_air * N_l_tot / N_l_tot_s # Max. rate at 20 degC

P_max   = P_max20 * f_T (T_air) # Max rate of photosynthesis ([kg CO2] [m]-2 leaf [s]-1) 
# This P_max is an order of magnitude larger than P_max in Thornley and Johnson (1990)

f_T <- function (T_inp) {
  T_0   =  0.0e0 # Temperature at which rate processes cease ([degC])
  T_ref = 20.0e0 # Reference temperature at which f_T = 1 ([degC])
  T_max = 30.0e0 # Temperature at which rate processes are maximum ([degC])

  f_T   = ((T_inp - T_0) * (2 * T_max - T_0 - T_inp)) /   
  ((T_ref - T_0) * (2 * T_max - T_0 - T_ref)) 
  return (f_T)
}

alpha   = alpha_m * (1 - (beta / (tau * C_CO2_air))) # Calculate Leaf photosynthetic efficiency ([kg CO2][J]-1)

P_max   = 10.0e-6 # Parameters from Thorney and Johnson (1990), p. 228
alpha   = 10.0e-8 # Parameters from Thorney and Johnson (1990), p. 228
#I_leaf  = (k * I) / (1 - shi_leaf) * exp (-k * L)    # Calculate light flux density incident on leaf ([J] [m]-2 [s]-1)
I_leaf <- function (x) {
  I_leaf  = (k * x) / (1 - shi_leaf) * exp (-k * L)
  #I_leaf  = x * exp (-k * (n_stems * A_l))
  return (I_leaf)
}
layout (1)
curve (I_leaf, from = 0, to = 220.0)


# For leaf-level photosynthesis resolve the following:
# O = theta * P_g**2 - P_g * (alpha * I_leaf + P_max) + alpha * P_max for P_g
#-------------------------------------------------------------------------------------!
#P_l = 1 / (2 * theta) * (alpha * I_leaf + P_max - sqrt ((alpha * I_leaf + P_max)**2.0 - 4 * theta * alpha * I_leaf * P_max)) # ([kg CO2] [m]-2 leaf [s]-1)
P_l <- function (x) {
  P_l =  1 / (2 * theta) * (alpha * x + P_max - sqrt ((alpha * x + P_max)**2.0 - 4 * theta * alpha * x * P_max))
  return (P_l)
}
curve (P_l (I_leaf (x)), from = 0, to = 800)

theta = 0.999999999999
curve (P_l, from = 0.0, to = 400.0)
theta = 0.85
curve (P_l, from = 0.0, to = 400.0, add = T)
theta = 0.9
curve (P_l, from = 0.0, to = 400.0, add = T)
theta = 0.5
curve (P_l, from = 0.0, to = 400.0, add = T)
theta = 0.000000000001
curve (P_l, from = 0.0, to = 400.0, add = T)

#P_c     = P_l * ((1.0 - exp (-k * L)) / k) # Uptake of CO2 ([kg CO2] [m]-2 [s]-1)  
P_c <- function (x) {
  P_c = P_l (I_leaf (x)) * ((1.0 - exp (-k * x)) / k)
  return (P_c)
}
curve (P_c, from = 0, to = 800)

#P_carb  = (12.0 / 44.0) * ((h * P_c) / (n_stems)) # Uptake of C   ([kg C] [stem]-1 [d]-1)
P_carb <- function (x) {
  P_carb  = (12.0 / 44.0) * ((h * P_c (x)) / (n_stems))
  return (P_carb)
}
curve (P_carb, from = 0, to = 8)
#========================================================================================#