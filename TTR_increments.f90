!========================================================================================!
subroutine increments
!----------------------------------------------------------------------------------------!
 use TTR_parameters
 use TTR_variables
 !---------------------------------------------------------------------------------------!
 implicit none
 !---------------------------------------------------------------------------------------!

   !-------------------------------------------------------------------------------------!
   ! Timestep increments
   !-------------------------------------------------------------------------------------!
   ! For leaf area                                          ()
   !-------------------------------------------------------------------------------------!
   dA_l   = G_A_l   - L_A_l
   
   !-------------------------------------------------------------------------------------!
   ! For leaves                                             ()
   !-------------------------------------------------------------------------------------!
   dM_l_M = G_M_l_M - L_M_l_M_dif
   dM_l_X = G_M_l_X - L_M_l_X_lit + L_M_l_M_dif
   dM_l_C = P_carb  - T_C_l_b     - R_l_X_m     - U_C_l_G
   dM_l_N = T_N_b_l - U_N_l_G


   !-------------------------------------------------------------------------------------!
   ! For branches                                           ()
   !-------------------------------------------------------------------------------------!
   dM_b_M = G_M_b_M - L_M_b_M_dif
   dM_b_X = G_M_b_X - L_M_b_X_lit + L_M_b_M_dif
   dM_b_C = T_C_l_b - T_C_b_s     - R_b_X_m     - U_C_b_G
   dM_b_N = T_N_s_b - T_N_b_l     - U_N_b_G


   !-------------------------------------------------------------------------------------!
   ! For stems                                              ()
   !-------------------------------------------------------------------------------------!
   dM_s_M = G_M_s_M - L_M_s_M_dif
   dM_s_X = G_M_s_X - L_M_s_X_lit + L_M_s_M_dif
   dM_s_C = T_C_b_s - T_C_s_c     - R_s_X_m     - U_C_s_G
   dM_s_N = T_N_c_s - T_N_s_b     - U_N_s_G


   !-------------------------------------------------------------------------------------!
   ! For coarse roots                                       ()
   !-------------------------------------------------------------------------------------!
   dM_c_M = G_M_c_M - L_M_c_M_dif
   dM_c_X = G_M_c_X - L_M_c_X_lit + L_M_c_M_dif
   dM_c_C = T_C_s_c - T_C_c_f     - R_c_X_m     - U_C_c_G
   dM_c_N = T_N_f_c - T_N_c_s     - U_N_c_G


   !-------------------------------------------------------------------------------------!
   ! For fine roots and mycorrhiza                          ()
   !-------------------------------------------------------------------------------------!
   dM_f_M = G_M_f_M - L_M_f_M_dif
   dM_f_X = G_M_f_X - L_M_f_X_lit + L_M_f_M_dif
   dM_f_C = T_C_c_f - R_f_X_m     - U_C_f_G     - R_U_N
   dM_f_N = U_N     - T_N_f_c     - U_N_f_G


   !-------------------------------------------------------------------------------------! 
   ! Update pools
   !-------------------------------------------------------------------------------------!
   ! Leaf area                                              ()
   !-------------------------------------------------------------------------------------!
   A_l = A_l + dA_l
   
   
   !-------------------------------------------------------------------------------------! 
   ! Structure                                              ()
   !-------------------------------------------------------------------------------------!            
   M_l_X = M_l_X + dM_l_X
   M_b_X = M_b_X + dM_b_X
   M_s_X = M_s_X + dM_s_X
   M_c_X = M_c_X + dM_c_X
   M_f_X = M_f_X + dM_f_X


   !-------------------------------------------------------------------------------------!
   ! Meristems                                              ()
   !-------------------------------------------------------------------------------------!            
   M_l_M = M_l_M + dM_l_M
   M_b_M = M_b_M + dM_b_M
   M_s_M = M_s_M + dM_s_M
   M_c_M = M_c_M + dM_c_M
   M_f_M = M_f_M + dM_f_M


   !-------------------------------------------------------------------------------------!
   ! Carbon                                                 ()
   !-------------------------------------------------------------------------------------!
   M_l_C = M_l_C + dM_l_C
   M_b_C = M_b_C + dM_b_C
   M_s_C = M_s_C + dM_s_C
   M_c_C = M_c_C + dM_c_C
   M_f_C = M_f_C + dM_f_C


   !-------------------------------------------------------------------------------------!
   ! Nitrogen                                               ()
   !-------------------------------------------------------------------------------------!            
   M_l_N = M_l_N + dM_l_N
   M_b_N = M_b_N + dM_b_N
   M_s_N = M_s_N + dM_s_N
   M_c_N = M_c_N + dM_c_N
   M_f_N = M_f_N + dM_f_N

      
   !-------------------------------------------------------------------------------------!
   ! Calculate C substrate concentrations                   ([kg C] [kg dm]-1)
   !-------------------------------------------------------------------------------------!          
   C_l = M_l_C / (M_l_X + M_l_M)                            ! In leaves
   C_b = M_b_C / M_b_M                                      ! In branches
   C_s = M_s_C / M_s_M                                      ! In stems
   C_c = M_c_C / M_c_M                                      ! In coarse roots
   C_f = M_f_C / (M_f_X + M_f_M)                            ! In fine roots and mycorrhiza


   !-------------------------------------------------------------------------------------!   
   ! Calculate N substrate concentrations                  ([kg C] [kg dm]-1)
   !-------------------------------------------------------------------------------------!          
   N_l = M_l_N / (M_l_X + M_l_M)                           ! In leaves
   N_b = M_b_N / M_b_M                                     ! In branches
   N_s = M_s_N / M_s_M                                     ! In stems
   N_c = M_c_N / M_c_M                                     ! In coarse roots
   N_f = M_f_N / (M_f_X + M_f_M)                           ! In fine roots and mycorrhiza

   
   !-------------------------------------------------------------------------------------!   
   ! Calculate total N concentration                       ([kg total N] [kg X dm]-1)
   !-------------------------------------------------------------------------------------!          
   N_l_tot = (M_l_N + f_N_l_X * M_l_X + f_N_l_M * M_l_M) / (M_l_X + M_l_M)

   
   !-------------------------------------------------------------------------------------!   
   ! Mortality - substrate concentration is negative!
   !-------------------------------------------------------------------------------------!   
   if (C_l < epsilon) then
     stop 'Oh my, it is dead! There is not enough leaf carbon!'
   else if (C_b < epsilon) then
     stop 'Oh my, it is dead! There is not enough branch carbon!'
   else if (C_s < epsilon) then
     stop 'Oh my, it is dead! There is not enough stem carbon!'
   else if (C_c < epsilon) then
     stop 'Oh my, it is dead! There is not enough coarse roots carbon!'
   else if (C_f < epsilon) then
     stop 'Oh my, it is dead! There is not enough fine roots carbon!'
   else if (N_l < epsilon) then
     stop 'Oh my, it is dead! There is not enough leaf nitrogen!'
   else if (N_b < epsilon) then
     stop 'Oh my, it is dead! There is not enough branch nitrogen!'
   else if (N_s < epsilon) then
     stop 'Oh my, it is dead! There is not enough stem nitrogen!'
   else if (N_c < epsilon) then
     stop 'Oh my, it is dead! There is not enough coarse root nitrogen!'
   else if (N_f < epsilon) then
     stop 'Oh my, it is dead! There is not enough fine root nitrogen!'
   end if
   
   
!----------------------------------------------------------------------------------------!
end subroutine increments
!========================================================================================!
      