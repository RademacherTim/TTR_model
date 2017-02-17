!========================================================================================!
subroutine C_uptake
!----------------------------------------------------------------------------------------!

   !-------------------------------------------------------------------------------------!
   ! Light interception and photosynthesis
   !-------------------------------------------------------------------------------------!
   L       = n_stems * A_t ! Calculate plot leaf area index   ([m]2 leaf [m]2 ground)
   I       = J / h         ! Calculate instantaneous light flux density ([J] [m]-2)
   P_max20 = tau * C_CO2_air * N_l_tot / N_l_tot_s ! Max. rate at 20 degC
   P_max   = P_max20 * f_T (T_air) ! Max rate of photosynthesis ()
   alpha   = alpha_m * (1 - (beta / (tau * C_CO2_air))) ! Calculate Leaf photosynthetic efficiency ([kg CO2][J]-1)
   I_leaf  = (k * I) / (1 - Shi_leaf) * exp (-k * L)    ! Calculate light flux density incident on leaf ([J] [m]-2 [s]-1)
   ! Resolve O = theta * P_g**2 - P_g * (alpha * I_leaf + P_max) + alpha * P_max for P_g
   P_g1    = -(alpha * I_leaf + P_max) +                                                 &
              (sqrt ((alpha * I_leaf + P_max) - 4.0 * theta * alpha * P_max) ) /         & 
              (2.0 * theta)
   P_g2    = -(alpha * I_leaf + P_max) -                                                 &
              (sqrt ((alpha * I_leaf + P_max) - 4.0 * theta * alpha * P_max) ) /         &
              (2.0 * theta)
   P_g     = max (P_g1, P_g2) ! ([kg CO2] [m]-2 leaf [s]-1)
   P_CO2   = P_g * L
   P_carb  = (12.0 / 44.0) * ((h * P_CO2) / (n_stems)) ! Scale to ([kg C] [stem]-1 [d]-1)
   !-------------------------------------------------------------------------------------!
   write (*, '(a15, 11a12)') 'Photosynthesis ','L','I','P_max20','P_max','alpha',        &
                             'I_leaf','P_g1','P_g2','P_g','P_CO2','P_carb' 
   write (*, '(a15, 11f12.5)') 'Photosynthesis ', L, I, P_max20, P_max, alpha, I_leaf,   &
                                                  P_g1, P_g2, P_g, P_CO2, P_carb * 100.0
   write (*, *)
   ! TTR Not sure photosynthesis is correct!!!! Especially P_g derivation!

!----------------------------------------------------------------------------------------!
end subroutine C_uptake
!========================================================================================!