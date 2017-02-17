!========================================================================================!
subroutine growth 
!----------------------------------------------------------------------------------------!
 use TTR_parameters
 use TTR_variables
 !---------------------------------------------------------------------------------------!
 implicit none
 !---------------------------------------------------------------------------------------!

   !-------------------------------------------------------------------------------------!
   ! Growth (G) of meristematic and structure dry matter for leaves
   !-------------------------------------------------------------------------------------!
   ! Calculate the growth coefficients 
   !-------------------------------------------------------------------------------------!
   k_l_M     = k_l_M20 * f_T_air
   k_b_M     = k_b_M20 * f_T_air
   k_s_M     = k_s_M20 * f_T_air
   k_c_M     = k_c_M20 * f_T_soil
   k_f_M     = k_f_M20 * f_T_soil
   !write (*, *) 'k    ',k_l_M,k_b_M,k_s_M,k_c_M,k_f_M, f_T_air, f_T_soil
   !-------------------------------------------------------------------------------------!
   ! Calculate growth coefficient closer to Mitscherlich's law of constant activity
   !-------------------------------------------------------------------------------------! 
   if (.not. LIEBIG) then
     mu_l_M    = k_l_M * C_l * N_l 
     mu_b_M    = k_b_M * C_b * N_b
     mu_s_M    = k_s_M * C_s * N_s
     mu_c_M    = k_c_M * C_c * N_c
     mu_f_M    = k_f_M * C_f * N_f
   !-------------------------------------------------------------------------------------!
   ! Calculate growth coefficient according to Liebig's law of the minimum
   !-------------------------------------------------------------------------------------! 
   else if (LIEBIG) then
     mu_l_M    = k_l_M * (C_l - C_l_threshold) * (N_l - N_l_threshold)
     mu_b_M    = k_b_M * (C_b - C_b_threshold) * (N_b - N_b_threshold)
     mu_s_M    = k_s_M * (C_s - C_s_threshold) * (N_s - N_s_threshold)
     mu_c_M    = k_c_M * (C_c - C_c_threshold) * (N_c - N_c_threshold)
     mu_f_M    = k_f_M * (C_f - C_f_threshold) * (N_f - N_f_threshold)
   end if   
   !write (*, *) mu_l_M, k_l_M, C_l, N_l
   !write (*, *) 'mu   ', mu_l_M,mu_b_M,mu_s_M,mu_c_M,mu_f_M
   !-------------------------------------------------------------------------------------!   
   ! Calculate the surface area for branches, stems and coarse roots
   !-------------------------------------------------------------------------------------! 
   A_b       = c_A_b * M_b_X**q_A_b
   A_s       = c_A_s * M_s_X**q_A_s
   A_c       = c_A_c * M_c_X**q_A_c
   !write (*, *) 'A    ', A_b, A_s, A_c
   !-------------------------------------------------------------------------------------!
   ! Calculate the potential meristem size
   !-------------------------------------------------------------------------------------! 
   M_l_M_pot = C_l_M_pot * A_b * C_l * N_l
   M_b_M_pot = C_b_M_pot * A_b * C_b * N_b
   M_s_M_pot = C_s_M_pot * A_s * C_s * N_s
   M_c_M_pot = C_c_M_pot * A_c * C_c * N_c
   M_f_M_pot = C_f_M_pot * A_c * C_f * N_f
   !write (*, *) 'M_pot', M_l_M_pot,M_b_M_pot,M_s_M_pot,M_c_M_pot,M_f_M_pot
   !-------------------------------------------------------------------------------------!
   ! Determine growth allocation to meristem and structure
   !-------------------------------------------------------------------------------------! 
   z_l       = min ((M_l_M_pot / M_l_M - 1.0), 1.0)
   z_b       = min ((M_b_M_pot / M_b_M - 1.0), 1.0)
   z_s       = min ((M_s_M_pot / M_s_M - 1.0), 1.0)
   z_c       = min ((M_c_M_pot / M_c_M - 1.0), 1.0)
   z_f       = min ((M_f_M_pot / M_f_M - 1.0), 1.0)
   write (*, '(a6, 5f8.3)') 'z     ', z_l,z_b,z_s,z_c,z_f  
   !-------------------------------------------------------------------------------------!
   ! Calculate growth of meristems
   !-------------------------------------------------------------------------------------! 
   G_M_l_M   = mu_l_M * z_l * M_l_M
   G_M_b_M   = mu_b_M * z_b * M_b_M 
   G_M_s_M   = mu_s_M * z_s * M_s_M
   G_M_c_M   = mu_c_M * z_c * M_c_M
   G_M_f_M   = mu_f_M * z_f * M_f_M
   !write (*, *) 'G_M  ',G_M_l_M,G_M_b_M,G_M_s_M,G_M_c_M,G_M_f_M
   !-------------------------------------------------------------------------------------!   
   ! Calculate growth of structure
   !-------------------------------------------------------------------------------------! 
   G_M_l_X   = mu_l_M * (1.0 - z_l) * M_l_M
   G_M_b_X   = mu_b_M * (1.0 - z_b) * M_b_M 
   G_M_s_X   = mu_s_M * (1.0 - z_s) * M_s_M
   G_M_c_X   = mu_c_M * (1.0 - z_c) * M_c_M 
   G_M_f_X   = mu_f_M * (1.0 - z_f) * M_f_M
   !write (*, *) G_M_l_X, mu_l_M, z_f, M_l_M
   !write (*, *) 'G_X  ',G_M_l_X,G_M_b_X,G_M_s_X,G_M_c_X,G_M_f_X
   !write (*, *) G_M_l_M + G_M_l_X, M_l_C, G_M_b_M + G_M_b_X, M_b_C, G_M_s_M + G_M_s_X,   &
   !             M_s_C, G_M_c_M + G_M_c_X, M_c_C, G_M_f_M + G_M_f_X, M_f_C
   !-------------------------------------------------------------------------------------!    
   ! Calculate the growth of the leave area
   !-------------------------------------------------------------------------------------! 
   c_a_l_a   = c_a_l_amx * (1.0 - c_a_l_a_C * C_l)
   G_A_l     = c_a_l_a * G_M_l_X
   !write (*, *) 'G_A_l',G_A_l
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 11a12)')   ' ','G_A_l', 'G_M_l_M','G_M_l_X','G_M_b_M','G_M_b_X',    &
   !                            'G_M_s_M','G_M_s_X','G_M_c_M','G_M_c_X','G_M_f_M','G_M_f_X'
   !write (*, '(a15, 11f12.5)') 'Growth  ', G_A_l, G_M_l_M, G_M_l_X, G_M_b_M, G_M_b_X,      &
   !                             G_M_s_M, G_M_s_X, G_M_c_M, G_M_c_X, G_M_f_M, G_M_f_X
   !write (*, *)  

   !-------------------------------------------------------------------------------------!
   ! Convert to growth per timestep 
   !-------------------------------------------------------------------------------------!
    G_M_l_M = G_M_l_M * dt
    G_M_b_M = G_M_b_M * dt
    G_M_s_M = G_M_s_M * dt
    G_M_c_M = G_M_c_M * dt
    G_M_f_M = G_M_f_M * dt
    G_M_l_X = G_M_l_X * dt
    G_M_b_X = G_M_b_X * dt
    G_M_s_X = G_M_s_X * dt
    G_M_c_X = G_M_c_X * dt
    G_M_f_X = G_M_f_X * dt
    
!----------------------------------------------------------------------------------------!
end subroutine growth
!========================================================================================!