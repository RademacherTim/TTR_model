!========================================================================================!
subroutine C_uptake 
!----------------------------------------------------------------------------------------!
 use TTR_parameters
 use TTR_variables
 !---------------------------------------------------------------------------------------!
 implicit none
 !---------------------------------------------------------------------------------------!
 
   !-------------------------------------------------------------------------------------!
   ! Light interception and photosynthesis
   !-------------------------------------------------------------------------------------!
   L       = n_stems * A_l ! Calculate plot leaf area index   ([m]2 leaf [m]2 ground)
   I       = J / h         ! Calculate instantaneous light flux density ([J] [m]-2)
   P_max20 = tau * C_CO2_air * N_l_tot / N_l_tot_s ! Max. rate at 20 degC
   !write (*, *) P_max20, tau, C_CO2_air, N_l_tot, N_l_tot_s
   P_max   = P_max20 * f_T_air ! Max rate of photosynthesis ([kg CO2] [m]-2 leaf [s]-1)
   alpha   = alpha_m * (1 - (beta / (tau * C_CO2_air))) ! Calculate Leaf photosynthetic efficiency ([kg CO2][J]-1)
   I_leaf  = (k * I) / (1 - Shi_leaf) * exp (-k * L)    ! Calculate light flux density incident on leaf ([J] [m]-2 [s]-1)

   ! For leaf-level photosynthesis resolve the following:
   ! O = theta * P_g**2 - P_g * (alpha * I_leaf + P_max) + alpha * P_max for P_g
   !-------------------------------------------------------------------------------------!
   P_l     = 1 / (2 * theta) * (alpha * I_leaf + P_max - sqrt ((alpha * I_leaf + P_max)**2.0 - 4 * theta * alpha * I_leaf * P_max)) ! ([kg CO2] [m]-2 leaf [s]-1)
   P_c     = P_l * ((1.0 - exp (-k * L)) / k) ! Uptake of CO2 ([kg CO2] [m]-2 [s]-1)                             
   P_carb  = (12.0 / 44.0) * ((h * P_c) / (n_stems)) ! Uptake of C   ([kg C] [stem]-1 [d]-1)
   !-------------------------------------------------------------------------------------!
   ! TTR Not sure photosynthesis is correct!!!! Especially P_g derivation!
   ! P_c tends to be slightly underestimated using this approach according to Thornley 
   ! and Johnson (1990), p. 249

   ! Convert to uptake per timestep
   !-------------------------------------------------------------------------------------!
   !write (*, *) P_carb, dt, h, P_CO2, n_stems
   P_carb = P_carb * dt   
   
   
!----------------------------------------------------------------------------------------!
end subroutine C_uptake
!========================================================================================!