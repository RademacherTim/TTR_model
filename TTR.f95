!========================================================================================!
! 
! Thornley-Transport-Resistance Model (TTR) for trees
!
! original idea after:
!
!	J. Thornley (1991) , 'A Transport-resistance Model of Forest Growth and Partitioning',
!   Annals of Botany, 68, 211 - 226
!
! 	J. Thornley (1998), 'Modeling Shoot:Root Relations: the Only Way Forward?', Annals 
! 	of Botany, 81, 165 - 171
!
! further developed by :
!	Tim Tito Rademacher (email: rademacher.tim@gmail.com or trademacher@fas.harvard.edu)
!   during a postdoc at Harvard under Andrew D. Richardson
!
! last update (updates are listed in logfile_TTR):
! 	01/27/2017
!
! Definitions and symbols:
! 	C  denotes carbon
!   N  denotes nitrogen
!	X  denotes structural dry matter
!   dm denotes dry matter 
!   _l denotes in foliage
!   _b denotes in branches
!   _s denotes in stems
!   _c denotes in coarse roots
!   _f denotes in fine roots and mycorrhiza
!   _X denotes structural, non-meristematic, non-substrate
!   _M denotes meristematic, non-substrate
!   _C denotes carbon substrate
!   _N denotes nitrogen substrate
! 
!----------------------------------------------------------------------------------------!
program TTR_model
 !---------------------------------------------------------------------------------------!
 use TTR_parameters
 use TTR_variables
 !---------------------------------------------------------------------------------------!
  
 !---------------------------------------------------------------------------------------!
 ! Initialise state variables
 !---------------------------------------------------------------------------------------!

 ! Foliage (subscript, l)
 !---------------------------------------------------------------------------------------!
 A_t   =   0.003d0 ! Foliage area             ([m]2 [stem]-1)
 M_l_C =   0.04d-3 ! Foliage C substrate      ([kg C] [stem]-1)
 M_l_M = 100.0d-6  ! Foliage meristem         ([kg dm] [stem])
 M_l_N =   0.01d-3 ! Foliage N substrate      ([kg N] [stem]-1)
 M_l_X =   0.001d0 ! Foliage structure        ([kg dm] [stem]-1)

 ! Branches (subscript, b)
 !---------------------------------------------------------------------------------------!
 M_b_C =  35.0d-6  ! Branch C substrate       ([kg C] [stem]-1)
 M_b_M = 100.0d-6  ! Branch meristem          ([kg dm] [stem])
 M_b_N =  15.0d-6  ! Branch N substrate       ([kg N] [stem]-1)
 M_b_X =   0.001d0 ! Branch structure         ([kg dm] [stem]-1)
 
 ! Stems (subscript, s)
 !---------------------------------------------------------------------------------------!
 M_s_C =   3.0d-3  ! Stem C substrate         ([kg C] [stem]-1)
 M_s_M = 100.0d-6  ! Stem meristem            ([kg dm] [stem])
 M_s_N =   2.0d-6  ! Stem N substrate         ([kg N] [stem]-1)
 M_s_X =   0.001d0 ! Stem structure           ([kg dm] [stem]-1)
 
 ! Coarse roots (subscript, c)
 !---------------------------------------------------------------------------------------!
 M_c_C =  25.0d-6  ! Coarse roots C substrate ([kg C] [stem]-1)
 M_c_M = 100.0d-6  ! Coarse roots meristem    ([kg dm] [stem])
 M_c_N =  25.0d-6  ! Coarse roots N substrate ([kg N] [stem]-1)
 M_c_X =   0.001d0 ! Coarse roots structure   ([kg dm] [stem]-1)
 
 ! Fine roots and mycorrhiza (subscript, f)
 !---------------------------------------------------------------------------------------!
 M_f_C =   0.02d-3 ! Fine roots C substrate   ([kg C] [stem]-1)
 M_f_M = 100.0d-6  ! Fine roots meristem      ([kg dm] [stem])
 M_f_N =   0.03d-3 ! Fine roots N substrate   ([kg N] [stem]-1)
 M_f_X =   0.001d0 ! Fine roots structure     ([kg dm] [stem]-1)

 !---------------------------------------------------------------------------------------!
 ! Initialise environmental variables
 !---------------------------------------------------------------------------------------!
 C_CO2  =    400.0d0     ! CO2 concentration               ([ppmv])
 J      =      5.0d6     ! Daily radiation recept          ([J] [m]-2 [d]-1)
 N_amm  =      0.00075d0 ! Soil mineral ammonium level     ([kg N] [m]-2)
 N_nit  =      0.00025d0 ! Soil mineral nitrate level      ([kg N] [m]-2)
 p_atm  = 101325.0d0     ! Mean daily atmospheric pressure ([Pa])
 T_air  =     14.0d0     ! Mean daily air temperature      ([degC])
 T_soil =     14.0d0     ! mean daily soil temperature     ([degC])
 h      = 50400          ! Day length                      ([s] [d]-1)

 !---------------------------------------------------------------------------------------!
 ! Initialise number of stems
 !---------------------------------------------------------------------------------------!
 n_stems = 1
 
 !---------------------------------------------------------------------------------------!
 ! Initialise number of timesteps
 !---------------------------------------------------------------------------------------!
 tstps = floor (years * 365.25)

 ! Open outputs files and save parameters for simulation
 !---------------------------------------------------------------------------------------!
 open  (20, file = 'tmp/pools.txt',          status = 'unknown', action = 'write') 
 open  (21, file = 'tmp/fluxes.txt',         status = 'unknown', action = 'write')
 open  (22, file = 'tmp/transport.txt',      status = 'unknown', action = 'write')
 open  (23, file = 'tmp/increments.txt',     status = 'unknown', action = 'write')
 open  (24, file = 'tmp/concentrations.txt', status = 'unknown', action = 'write')
 !---------------------------------------------------------------------------------------!
 open  (31, file = 'tmp/environment.txt',    status = 'unknown', action = 'write')
 write (31, '(8a14)') 'C_CO2 [ppm]','T_air [degC]','T_soil [degC]','p_atm [Pa]',             &
                      'J [J m-2 d-1]','h [s]','N_amm [kg m-2]','N_nit [kg m-2]'
 write (31, '(5f14.5, i14, 2f14.5)') C_CO2, T_air, T_soil, p_atm, J, h, N_amm, N_nit
 close (31) ! Close environment.txt output file
 
 ! Loop over timesteps
 !---------------------------------------------------------------------------------------!
 timestep : do t = 1, tstps
 
   ! Calculate CO2 concentration in ambient air
   !-------------------------------------------------------------------------------------!
   C_CO2_air = (C_CO2 / 10d6) * (273.15 / T_air + 273.15) * (p_atm / 101325) * 1.9636
   !write (*, *) C_CO2_air
 
   ! Calculate foliage substrate and total N concentration  ([kg C] [kg dm]-1)
   !-------------------------------------------------------------------------------------!          
   C_l   = M_l_C / (M_l_X + M_l_M)     ! C concentration
   N_l   = M_l_N / (M_l_X + M_l_M)     ! N concentration
   N_l_tot = (M_l_N + f_N_l_X * M_l_X + f_N_l_M * M_l_M) / (M_l_X + M_l_M)
   write (*, '(a15, 11f12.5)') 'Concentrations ', C_l, C_b, C_s, C_c, C_f, N_l, N_b, N_s,&
                                                  N_c, N_f, N_l_tot
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
   P_carb  = (12.0 / 44.0) * ((h * P_CO2) / (n_stems)) ! Scale to stem per day
   write (*, '(a15, 11f12.5)') 'Photosynthesis ', L, I, P_max20, P_max, alpha, I_leaf,   &
                                                    P_g1, P_g2, P_g, P_CO2, P_carb 
   ! TTR Not sure photosynthesis is correct!!!! Especially P_g derivation!
 
   ! Growth (G) of meristematic and structure dry matter
   !-------------------------------------------------------------------------------------!
   k_l_M     = k_l_M20 * f_T (T_air) 
   mu_l_M    = k_l_M * C_l * N_l 
   M_l_M_pot = C_l_M_pot * A_b * C_l * N_l
   z_l       = min ((M_l_M_pot / M_l_M - 1.0), 1.0)
   G_M_l_M   = mu_l_M * z_l * M_l_M
   G_M_l_X   = mu_l_M * (1.0 - z_f) * M_l_M 
   c_a_l_a   = c_a_l_amx * (1.0 - c_a_l_a_C * C_l)
   G_A_l     = c_a_l_a * G_M_l_X
   write (*, '(a15, 8f12.5)') 'Leaf growth ', mu_l_M, k_l_M, M_l_M_pot, z_l, G_M_l_M,    &
                                              G_M_l_X, c_a_l_a, G_A_l
 
   ! Losses (L) to differentiation and to litter
   !-------------------------------------------------------------------------------------!
   k_dif_l     = k_dif_l20   * f_T (T_air)
   L_M_l_M_dif = k_dif_l     * M_l_M
   k_l_X_lit   = k_l_X_lit20 * f_T (T_air)
   L_M_l_X_lit = k_l_X_lit   * M_l_X
   k_A_l_lit   = k_A_l_lit20 * f_T (T_air)
   L_A_l       = k_A_l_lit   * A_l
   write (*, '(a15, 6f12.5)') 'Losses to litter', k_dif_l, L_M_l_M_dif, k_l_X_lit,       &
                                                  L_M_l_X_lit, k_A_l_lit, L_A_l
 
   ! Utilisation (U) of C and N substrates, and Respiration (R)
   !-------------------------------------------------------------------------------------!
   U_C_l_G = (f_C_l_M * G_M_l_M) / (Y_l_M) + (f_C_l_X * G_M_l_X) / (Y_l_X) 
   U_N_l_G = f_N_l_M * G_M_l_M + f_N_l_X * G_M_l_X
   m_M_l_X = m_M_l_X20 * f_T (T_air)
   R_l_X_m = (m_M_l_X * f_C_l_X * M_l_X) * (C_l) / (C_l + K_m_l_C)
   R_l_X_G = ((1.0 - Y_l_M) * f_C_l_M * G_M_l_M) / (Y_l_M) +                             &
             ((1.0 - Y_l_X) * f_C_l_X * G_M_l_X) / (Y_l_X)
   write (*, '(a15, 4f12.5)') 'Util. and Resp. ', U_C_l_G, U_N_l_G, R_l_X_m, R_l_X_G
 
   ! Transport fluxes (T) of C between foliage and branches
   !-------------------------------------------------------------------------------------!
   c_tau_C_l_b = c_tau_C_l_b20 * f_T (T_air)
   g_C_l_b     = c_tau_C_l_b * (((M_l_X + M_l_M) * M_b_M) /                             &
                                 (M_l_X + M_l_M + M_b_M))
   T_C_l_b     = g_C_l_b * (C_l - C_b)
   write (*, '(a15, 3f12.5)') 'Leaf C transport ', c_tau_C_l_b, g_C_l_b, T_C_l_b
 
   ! Transport fluxes (T) of C between foliage and branches
   !-------------------------------------------------------------------------------------!
   c_tau_N_b_l = c_tau_N_b_l20 * f_T (T_air)
   g_N_b_l     = c_tau_N_b_l * ((M_b_M * (M_l_M + M_l_X)) /                             &
                                (M_b_M + M_l_M + M_l_X))
   T_N_b_l     = g_N_b_l * (N_b - N_l)
   write (*, '(a15, 3f12.5)') 'Leaf N transport ', c_tau_N_b_l, g_N_b_l, T_N_b_l
 
   ! Differential equations of leaves
   !-------------------------------------------------------------------------------------!
   dA_l   = G_A_l   - L_A_l
   dM_l_M = G_M_l_M - L_M_l_M_dif
   dM_l_X = G_M_l_X - L_M_l_X_lit + L_M_l_M_dif
   dM_l_C = P_carb  - T_C_l_b     - R_l_X_m     - U_C_l_G
   dM_l_N = T_N_b_l - U_N_l_G
   write (*, '(a15, 5f12.5)') 'Changes ', dA_l, dM_l_M, dM_l_X, dM_l_C, dM_l_N
 
   ! Branches substrate concentrations
   !-------------------------------------------------------------------------------------!
   C_b = M_b_C / M_b_M
   N_b = M_b_N / M_b_M
   write (*, '(a15, 2f12.5)') 'Branch sub [] ', C_b, N_b
 
   ! Growth (G) of meristem (M) and structural (X) dry matter
   !-------------------------------------------------------------------------------------!
   k_b_M     = k_b_M20 * f_T (T_air) 
   mu_b_M    = k_b_M * C_b * N_b 
   M_b_M_pot = C_b_M_pot * A_b * C_b * N_b
   z_b       = min ((M_b_M_pot / M_b_M - 1.0), 1.0)
   G_M_b_M   = mu_b_M * z_b * M_b_M
   G_M_b_X   = mu_b_M * (1.0 - z_b) * M_b_M 
   A_b       = c_A_b * M_b_X**q_A_b
   write (*, '(a15, 8f12.5)') 'Branch growth ', mu_b_M, k_b_M, M_b_M_pot, z_b, G_M_b_M,  &
                                                G_M_b_X, A_b
                                            
   ! Losses (L) to differentiation and to litter
   !-------------------------------------------------------------------------------------!
   k_dif_b     = k_dif_b20   * f_T (T_air)
   L_M_b_M_dif = k_dif_b     * M_b_M
   k_b_X_lit   = k_b_X_lit20 * f_T (T_air)
   L_M_b_X_lit = k_b_X_lit   * M_b_X
   write (*, '(a15, 4f12.5)') 'Losses to litter', k_dif_b, L_M_b_M_dif, k_b_X_lit,       &
                                                  L_M_b_X_lit
                                                
   ! Utilisation (U) of C and N substrates, and Respiration (R)
   !-------------------------------------------------------------------------------------!
   U_C_b_G = (f_C_b_M * G_M_b_M) / (Y_b_M) + (f_C_b_X * G_M_b_X) / (Y_b_X) 
   U_N_b_G = f_N_b_M * G_M_b_M + f_N_b_X * G_M_b_X
   m_A_b   = m_A_b20 * f_T (T_air)
   R_b_X_m = m_A_b * (C_b) / (C_b + K_m_b_C)
   R_b_X_G = ((1.0 - Y_b_M) * f_C_b_M * G_M_b_M) / (Y_b_M) +                             &
             ((1.0 - Y_b_X) * f_C_b_X * G_M_b_X) / (Y_b_X)
   write (*, '(a15, 4f12.5)') 'Util. and Resp. ', U_C_b_G, U_N_b_G, R_b_X_m, R_b_X_G   

   ! Transport fluxes (T) of C between branches (b) and foliage (l), and stems (s)
   !-------------------------------------------------------------------------------------!
   c_tau_C_b_s = c_tau_C_b_s20 * f_T (T_air)
   g_C_b_s     = c_tau_C_b_s * ((M_b_M + M_s_M) /                                        &
                                (M_l_M + M_s_M))
   T_C_b_s     = g_C_b_s * (C_b - C_s)
   write (*, '(a15, 3f12.5)') 'Branch C transport ', c_tau_C_b_s, g_C_b_s, T_C_b_s
 
 
   ! Transport fluxes (T) of C between foliage and branches
   !-------------------------------------------------------------------------------------!
   c_tau_N_b_l = c_tau_N_b_l20 * f_T (T_air)
   g_N_b_l     = c_tau_N_b_l * ((M_b_M * (M_l_M + M_l_X)) /                             &
                                (M_b_M + M_l_M + M_l_X))
  T_N_b_l     = g_N_b_l * (N_b - N_l)
   write (*, '(a15, 3f12.5)') 'Branch N transport ', c_tau_N_b_l, g_N_b_l, T_N_b_l
 
   ! Differential equations of branches
   !-------------------------------------------------------------------------------------!
   dM_b_M = G_M_b_M - L_M_b_M_dif
   dM_b_X = G_M_b_X - L_M_b_X_lit + L_M_b_M_dif
   dM_b_C = T_C_b_s - R_b_X_m     - U_C_b_G
   dM_b_N = T_N_s_b - T_N_b_l     - U_N_b_G
   write (*, '(a15, 4f12.5)') 'Changes ', dM_b_M, dM_b_X, dM_b_C, dM_b_N
 
   ! Stems substrate concentrations
   !-------------------------------------------------------------------------------------!
   C_s = M_s_C / M_s_M
   N_s = M_s_N / M_s_M
   write (*, '(a15, 2f12.5)') 'Stem sub [] ', C_s, N_s
 
   ! Growth (G) of meristem (M) and structural (X) dry matter
   !-------------------------------------------------------------------------------------!
   k_s_M     = k_s_M20 * f_T (T_air)
   mu_s_M    = k_s_M * C_s * N_s 
   M_s_M_pot = c_s_M_pot * A_s * C_s * N_s
   z_s       = min ((M_s_M_pot / M_s_M - 1.0), 1.0)
   G_M_s_M   = mu_s_M * z_s * M_s_M
   G_M_s_X   = mu_s_M * (1.0 - z_s) * M_s_M 
   A_s       = c_A_s * M_s_X**q_A_s
   write (*, '(a15, 8f12.5)') 'Stem growth ', mu_s_M, k_s_M, M_s_M_pot, z_s, G_M_s_M,    &
                                              G_M_s_X, A_s
                                             
   ! Losses (L) to differentiation and to litter
   !-------------------------------------------------------------------------------------!
   k_dif_s     = k_dif_s20   * f_T (T_air)
   L_M_s_M_dif = k_dif_s     * M_s_M
   k_s_X_lit   = k_s_X_lit20 * f_T (T_air)
   L_M_s_X_lit = k_s_X_lit   * M_s_X
   write (*, '(a15, 4f12.5)') 'Losses to litter', k_dif_s, L_M_s_M_dif, k_s_X_lit,         &
                                                  L_M_s_X_lit
                                                
   ! Utilisation (U) of C and N substrates, and Respiration (R)
   !-------------------------------------------------------------------------------------!
   U_C_s_G = (f_C_s_M * G_M_s_M) / (Y_s_M) + (f_C_s_X * G_M_s_X) / (Y_s_X) 
   U_N_s_G = f_N_s_M * G_M_s_M + f_N_s_X * G_M_s_X
   m_A_s   = m_A_s20 * f_T (T_air)
   R_s_X_m = m_A_s * A_s * (C_s) / (C_s + K_m_s_C)
   R_s_X_G = ((1.0 - Y_s_M) * f_C_s_M * G_M_s_M) / (Y_s_M) +                             &
             ((1.0 - Y_s_X) * f_C_s_X * G_M_s_X) / (Y_s_X)
   write (*, '(a15, 4f12.5)') 'Util. and Resp. ', U_C_s_G, U_N_s_G, R_s_X_m, R_s_X_G   

   ! Transport fluxes (T) of C between branches (b) and stems (s), and coarse roots (c)
   !-------------------------------------------------------------------------------------!
   c_tau_C_s_c = c_tau_C_s_c20 * f_T (T_air)
   g_C_s_c     = c_tau_C_s_c * ((M_s_M + M_b_M) /                                        &
                                (M_s_M + M_b_M))
   T_C_s_c     = g_C_s_c * (C_s - C_c)
   write (*, '(a15, 3f12.5)') 'Stem C transport ', c_tau_C_s_c, g_C_s_c, T_C_s_c
 
 
   ! Transport fluxes (T) of C between branches and stems
   !-------------------------------------------------------------------------------------!
   c_tau_N_s_b = c_tau_N_s_b20 * f_T (T_air)
   g_N_s_b     = c_tau_N_s_b * ((M_s_M * M_b_M) /                                        &
                                (M_s_M + M_b_M))
   T_N_s_b     = g_N_s_b * (N_s - N_b)
   write (*, '(a15, 3f12.5)') 'Stem N transport ', c_tau_N_s_b, g_N_s_b, T_N_s_b
 
   ! Differential equations of stems
   !-------------------------------------------------------------------------------------!
   dM_s_M = G_M_s_M - L_M_s_M_dif
   dM_s_X = G_M_s_X - L_M_s_X_lit + L_M_s_M_dif
   dM_s_C = T_C_b_s - T_C_s_c     - R_s_X_m     - U_C_s_G
   dM_s_N = T_N_c_s - T_N_s_b     - U_N_s_G
   write (*, '(a15, 4f12.5)') 'Changes ', dM_s_M, dM_s_X, dM_s_C, dM_s_N
 
   ! Coarse roots substrate concentrations
   !-------------------------------------------------------------------------------------!
   C_c = M_c_C / M_c_M
   N_c = M_c_N / M_c_M
   write (*, '(a15, 2f12.5)') 'Coarse root sub [] ', C_c, N_c
 
   ! Growth (G) of meristem (M) and structural (X) dry matter
   !-------------------------------------------------------------------------------------!
   k_c_M     = k_c_M20 * f_T (T_soil) 
   mu_c_M    = k_c_M * C_c * N_c 
   M_c_M_pot = c_c_M_pot * A_c * C_c * N_c
   z_c       = min ((M_c_M_pot / M_c_M - 1.0), 1.0)
   G_M_c_M   = mu_c_M * z_c * M_c_M
   G_M_c_X   = mu_c_M * (1.0 - z_c) * M_c_M 
   A_c       = c_A_c * M_c_X**q_A_c
   write (*, '(a15, 8f12.5)') 'Coarse root growth ', mu_c_M, k_c_M, M_c_M_pot, z_c,      &
                                                     G_M_c_M, G_M_c_X, A_c
                                             
   ! Losses (L) to differentiation and to litter
   !-------------------------------------------------------------------------------------!
   k_dif_c     = k_dif_c20   * f_T (T_soil)
   L_M_c_M_dif = k_dif_c     * M_c_M
   k_c_X_lit   = k_c_X_lit20 * f_T (T_soil)
   L_M_c_X_lit = k_c_X_lit   * M_c_X
   write (*, '(a15, 4f12.5)') 'Losses to litter', k_dif_c, L_M_c_M_dif, k_c_X_lit,       &
                                                  L_M_c_X_lit
                                                
   ! Utilisation (U) of C and N substrates, and Respiration (R)
   !-------------------------------------------------------------------------------------!
   U_C_c_G = (f_C_c_M * G_M_c_M) / (Y_c_M) + (f_C_c_X * G_M_c_X) / (Y_c_X) 
   U_N_c_G = f_N_c_M * G_M_c_M + f_N_c_X * G_M_c_X
   m_A_c   = m_A_c20 * f_T (T_soil)
   R_c_X_G = ((1.0 - Y_c_M) * f_C_c_M * G_M_c_M) / (Y_c_M) +                             &
             ((1.0 - Y_c_X) * f_C_c_X * G_M_c_X) / (Y_c_X)
   write (*, '(a15, 4f12.5)') 'Util. and Resp. ', U_C_c_G, U_N_c_G, R_c_X_m, R_c_X_G   

   ! Transport fluxes (T) of C between coarse roots (c) and fine roots (f)
   !-------------------------------------------------------------------------------------!
   c_tau_C_c_f = c_tau_C_c_f20 * f_T (T_soil)
   g_C_c_f     = c_tau_C_c_f * ((M_c_M * (M_f_M + M_f_X)) /                              &
                                (M_c_M + M_f_M  + M_f_X))
   T_C_c_f     = g_C_c_f * (C_c - C_f)
   write (*, '(a15, 3f12.5)') 'Coarse root C transport ', c_tau_C_c_f, g_C_c_f, T_C_c_f
 
 
   ! Transport fluxes (T) of C between stems and coarse roots
   !-------------------------------------------------------------------------------------!
   c_tau_N_c_s = c_tau_N_c_s20 * ((f_T (T_soil) + f_T (T_soil)) / 2)
   g_N_c_s     = c_tau_N_c_s * ((M_c_M * M_s_M) /                                        &
                                (M_c_M + M_s_M))
   T_N_c_s     = g_N_c_s * (N_c - N_s)
   write (*, '(a15, 3f12.5)') 'Coarse root N transport ', c_tau_N_c_s, g_N_c_s, T_N_c_s
 
   ! Differential equations of coarse roots
   !-------------------------------------------------------------------------------------!
   dM_c_M = G_M_c_M - L_M_c_M_dif
   dM_c_X = G_M_c_X - L_M_c_X_lit + L_M_c_M_dif
   dM_c_C = T_C_s_c - T_C_c_f     - R_c_X_m     - U_C_c_G
   dM_c_N = T_N_f_c - T_N_c_s     - U_N_c_G
   write (*, '(a15, 4f12.5)') 'Changes ', dM_c_M, dM_c_X, dM_c_C, dM_c_N
 
   ! Fine roots and mycorrhiza substrate concentrations
   !-------------------------------------------------------------------------------------!
   C_f = M_f_C / (M_f_X + M_f_M)
   N_f = M_f_N / (M_f_X + M_f_M)
   write (*, '(a15, 2f12.5)') 'Fine root sub [] ', C_f, N_f
 
   ! N uptake by the fine roots and mycorrhiza
   !-------------------------------------------------------------------------------------!
   c_U_N   = c_U_N20 - (c_U_N20 - c_U_N10) * ((20.0 - T_soil) / (20.0 - 10.0))
   sigma_N = sigma_N20 * T_soil
   N_eff   = N_amm + c_U_N * N_nit
   U_N     = (M_f_X * sigma_N * N_eff) /                                                 &
             (1.0 + (K_c_U_N / C_f) * (N_f / J_N_U_N))
 
   ! Growth (G) of meristem (M) and structural (X) dry matter
   !-------------------------------------------------------------------------------------!
   k_f_M     = k_f_M20 * f_T (T_soil) 
   mu_f_M    = k_f_M * C_f * N_f 
   M_f_M_pot = c_f_M_pot * A_c * C_f * N_f
   z_f       = min ((M_f_M_pot / M_f_M - 1.0), 1.0)
   G_M_f_M   = mu_f_M * z_f * M_f_M
   G_M_f_X   = mu_f_M * (1.0 - z_f) * M_f_M 
   write (*, '(a15, 6f12.5)') 'Fine root growth ', mu_f_M, k_f_M, M_f_M_pot, z_f,        &
                                                   G_M_f_M, G_M_f_X
                                             
   ! Losses (L) to differentiation and to litter
   !-------------------------------------------------------------------------------------!
   k_dif_f     = k_dif_f20   * f_T (T_soil)
   L_M_f_M_dif = k_dif_f     * M_f_M
   k_f_X_lit   = k_f_X_lit20 * f_T (T_soil)
   L_M_f_X_lit = k_f_X_lit   * M_f_X
   write (*, '(a15, 4f12.5)') 'Losses to litter', k_dif_f, L_M_f_M_dif, k_f_X_lit,         &
                                                L_M_f_X_lit
                                                
   ! Utilisation (U) of C and N substrates, and Respiration (R)
   !-------------------------------------------------------------------------------------!
   U_C_f_G = (f_C_f_M * G_M_f_M) / (Y_f_M) + (f_C_f_X * G_M_f_X) / (Y_f_X) 
   U_N_f_G = f_N_f_M * G_M_f_M + f_N_f_X * G_M_f_X
   m_M_f_X   = m_M_f_X20 * f_T (T_soil)
   R_f_X_m = m_M_f_X * f_C_f_X * M_f_X * (C_f / (C_f + K_m_f_C))
   R_f_X_G = ((1.0 - Y_f_M) * f_C_f_M * G_M_f_M) / (Y_f_M) +                             &
             ((1.0 - Y_f_X) * f_C_f_X * G_M_f_X) / (Y_f_X)
    U_N_nit = U_N * ((c_U_N * N_nit) / N_eff)
    U_N_amm = U_N * (N_amm / N_eff)
    R_U_N   = U_N_amm * c_U_N_amm + U_N_nit * c_U_N_nit

   write (*, '(a15, 7f12.5)') 'Util. and Resp. ', U_C_f_G, U_N_f_G, R_f_X_m, R_f_X_G,    & 
                                                  U_N_nit, U_N_amm, R_U_N
 
 
   ! Transport fluxes (T) of C between fine roots and mycorrhiza and coarse roots
   !-------------------------------------------------------------------------------------!
   c_tau_N_f_c = c_tau_N_f_c20 * f_T (T_soil)
   g_N_f_c     = c_tau_N_f_c * (((M_f_X + M_f_M) * M_c_M) /                              &
                                 (M_f_X + M_f_M + M_c_M))
   T_N_f_c     = g_N_f_c * (N_f - N_c)
   write (*, '(a15, 3f12.5)')'Fine root N transport ', c_tau_N_f_c, g_N_f_c, T_N_f_c
 
   ! Differential equations of fine roots and mycorrhiza
   !-------------------------------------------------------------------------------------!
   dM_f_M = G_M_f_M - L_M_f_M_dif
   dM_f_X = G_M_f_X - L_M_f_X_lit + L_M_f_M_dif
   dM_f_C = T_C_c_f - R_f_X_m     - U_C_f_G     - R_U_N
   dM_f_N = U_N     - T_N_f_c     - U_N_f_G
   write (*, '(a15, 4f12.5)') 'Changes ', dM_f_M, dM_f_X, dM_f_C, dM_f_N
 
 !---------------------------------------------------------------------------------------! 
 end do timestep ! End of timestep loop
 
 !---------------------------------------------------------------------------------------!
 close (20) ! Close pools.txt output file
 close (21) ! Close fluxes.txt output file
 close (22) ! Close transport.txt output file
 close (23) ! Close increments.txt output file
 close (24) ! Close concentrations.txt output file
  
 !---------------------------------------------------------------------------------------!
end program TTR_model
!========================================================================================!

!========================================================================================!
function f_T (T_inp)
 !---------------------------------------------------------------------------------------!
 integer, parameter :: dp = selected_real_kind (15)
 real (dp) :: f_T ! Temperature function (unitless)
 real (dp), intent (in) :: T_inp        ! Input temperature - either air or soil ([degC])
 real (dp), parameter :: T_0   =  0.0d0 ! Temperature at which rate processes cease ([degC])
 real (dp), parameter :: T_ref = 20.0d0 ! Reference temperature at which f_T = 1 ([degC])
 real (dp), parameter :: T_max = 30.0d0 ! Temperature at which rate processes are maximum ([degC])
 
 f_T   = ((T_inp - T_0) * (2 * T_max - T_0 - T_inp)) /                                   &
         ((T_ref - T_0) * (2 * T_max - T_0 - T_ref)) 
 !---------------------------------------------------------------------------------------!
 return
 !---------------------------------------------------------------------------------------!
end function f_T
!========================================================================================!

!========================================================================================!
!subroutine trapezoid_integration (n, end_val)
 !---------------------------------------------------------------------------------------!
! implicit none
 !---------------------------------------------------------------------------------------!
! integer, parameter :: dp = selected_real_kind (15)
! integer :: n 
! real (dp), intent (in) :: end_val
! real (dp), intent (in) :: integral, u, h
! integer (i)
  
! integral = 0.0
  
! do i = 0, n
!   u = (end_val * i) / n
  	
   ! Implement equation G.4
   !-------------------------------------------------------------------------------------!
!   if ((i == 0) .or. (i == n)) then 
!     integral = integral + integrant (u)
!   else
!     integral = interal + (2.0 * integrant (u)) 
!   end if 
! end do
 
! h = end_val / n
! integral = (h / 2.0) * integral
 !---------------------------------------------------------------------------------------!
!end subroutine trapezoid_integration
!========================================================================================!

!========================================================================================!
!function integrant (x) result (value)
 !---------------------------------------------------------------------------------------!
! implicit none
 !---------------------------------------------------------------------------------------!
! real (dp), intent (in) :: x
! real (dp), intent (in) :: value
  
! if (x < 0.00001) then 
!   x = 0.00001
! end if
 
! value = (x**4)*exp(x)/((exp(x)-1.0)**2
  
 !---------------------------------------------------------------------------------------!
!end function integrant
!========================================================================================!

!========================================================================================!
! This module declares variables for the simulations run by the TTR model as implemented 
! in TTR.f90
!----------------------------------------------------------------------------------------!
module TTR_variables
!----------------------------------------------------------------------------------------!
 use TTR_parameters
 
 !---------------------------------------------------------------------------------------!
 ! Declare state variables
 !---------------------------------------------------------------------------------------!

 ! Foliage (subscript, l)
 !---------------------------------------------------------------------------------------!
 real (dp) :: A_t   ! Foliage area             ([m]2 [stem]-1)
 real (dp) :: M_l_C ! Foliage C substrate      ([kg C] [stem]-1)
 real (dp) :: M_l_M ! Foliage meristem         ([kg dm] [stem])
 real (dp) :: M_l_N ! Foliage N substrate      ([kg N] [stem]-1) 
 real (dp) :: M_l_X ! Foliage structure        ([kg dm] [stem]-1)
 real (dp) :: L     ! Foliage area of plot     ([m]2)
 real (dp) :: A_l   ! Foliage surface area     ([m]2)

 ! Branches (subscript, b)
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_b_C ! Branch C substrate       ([kg C] [stem]-1)
 real (dp) :: M_b_M ! Branch meristem          ([kg dm] [stem])
 real (dp) :: M_b_N ! Branch N substrate       ([kg N] [stem]-1)
 real (dp) :: M_b_X ! Branch structure         ([kg dm] [stem]-1)
 real (dp) :: A_b   ! Surface area of branches ([m]2)
 
 ! Stems (subscript, s)
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_s_C ! Stem C substrate         ([kg C] [stem]-1)
 real (dp) :: M_s_M ! Stem meristem            ([kg dm] [stem])
 real (dp) :: M_s_N ! Stem N substrate         ([kg N] [stem]-1)
 real (dp) :: M_s_X ! Stem structure           ([kg dm] [stem]-1)
 real (dp) :: A_s   ! Surface area of the stem ([m]2 [stem]-1)
 
 ! Coarse roots (subscript, c)
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_c_C ! Coarse roots C substrate     ([kg C] [stem]-1)
 real (dp) :: M_c_M ! Coarse roots meristem        ([kg dm] [stem])
 real (dp) :: M_c_N ! Coarse roots N substrate     ([kg N] [stem]-1)
 real (dp) :: M_c_X ! Coarse roots structure       ([kg dm] [stem]-1)
 real (dp) :: A_c   ! Surface area of coarse roots ([m]2 [stem]-1)
 
 ! Fine roots and mycorrhiza (subscript, f)
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_f_C ! Fine roots C substrate   ([kg C] [stem]-1)
 real (dp) :: M_f_M ! Fine roots meristem      ([kg dm] [stem])
 real (dp) :: M_f_N ! Fine roots N substrate   ([kg N] [stem]-1)
 real (dp) :: M_f_X ! Fine roots structure     ([kg dm] [stem]-1)

 !---------------------------------------------------------------------------------------!
 ! Declare environmental driving variables
 !---------------------------------------------------------------------------------------!
 real (dp) :: C_CO2     ! CO2 concentration               ([ppmv])
 real (dp) :: C_CO2_air ! ? CO2 concentration      (?)
 real (dp) :: J         ! Daily radiation recept          ([J] [m]-2 [d]-1)
 real (dp) :: N_amm     ! Soil mineral ammonium level     ([kg N] [m]-2)
 real (dp) :: N_nit     ! Soil mineral nitrate level      ([kg N] [m]-2)
 real (dp) :: N_eff     ! Effective soil N concentration  ([kg N] [m]-2)
 real (dp) :: p_atm     ! Mean daily atmospheric pressure ([Pa])
 real (dp) :: T_air     ! Mean daily air temperature      ([degC])
 real (dp) :: T_soil    ! mean daily soil temperature     ([degC])
 integer :: h           ! Day length                      ([s] [d]-1)

 !---------------------------------------------------------------------------------------!
 ! Declare spatio-temporal variables
 !---------------------------------------------------------------------------------------!
 integer, parameter :: years = 100 ! Number of years in simulation          ([yr])
 integer :: t                      ! Timestep                               ([d])
 integer :: tstps                  ! Number of timesteps in simulation      ([d])
 integer :: n_stems                ! Number of stems     in simulation      (unitless)

 !---------------------------------------------------------------------------------------!
 ! Declare variables
 !---------------------------------------------------------------------------------------!
 real (dp) :: f_T        ! Temperature function                    (unitless)
 real (dp) :: f_T_air    ! Temperature function for air            (unitless)
 real (dp) :: f_T_soil   ! Temperature function for the soil       (unitless)
 real (dp) :: I          ! Incident radiative flux                 ([J] [m-]2)
 real (dp) :: I_leaf     ! Incident radiative flux on leaf         ([J] [m]-2)
 real (dp) :: P_max20    ! Maximal rate of photosynthesis at 20 degC ([kg CO2] [m]2 leaf [s]-1)
 real (dp) :: P_max      ! Light-saturate rate of photosynthesis   ([kg CO2] [m]2 leaf [s]-1)
 real (dp) :: alpha      ! Leaf photosynthetic efficiency          ([kg CO2] [J]-1)
 real (dp) :: P_g        ! Leaf gross photosynthetic rate          ([kg CO2] [m]2 leaf [s]-1)
 real (dp) :: P_g1, P_g2 ! Solutions to quadratic equation for P_g
 real (dp) :: P_CO2      ! Canopy gross photosynthetic rate             ([kg CO2] [m]-2 ground [s]-1)
 real (dp) :: P_carb     ! Canopy gross photosynthetic rate             ([kg CO2] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 ! Meristematic activity                         ([C]-1 [N]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: k_l_M      ! In leaves
 real (dp) :: k_b_M      ! In branches
 real (dp) :: k_s_M      ! In stems
 real (dp) :: k_c_M      ! In coarse roots
 real (dp) :: k_f_M      ! In fine roots
 !---------------------------------------------------------------------------------------!
 ! Intrinsic specific growth rates of meristems  ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: mu_l_M     ! In leaves
 real (dp) :: mu_b_M     ! In branches
 real (dp) :: mu_s_M     ! In stems
 real (dp) :: mu_c_M     ! In coarse roots
 real (dp) :: mu_f_M     ! In fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Potential mass of leaf meristem              ([kg dm])
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_l_M_pot  ! For leaves
 real (dp) :: M_b_M_pot  ! For branches
 real (dp) :: M_s_M_pot  ! For stems
 real (dp) :: M_c_M_pot	 ! For coarse roots
 real (dp) :: M_f_M_pot	 ! For fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Meristem partitioning factor for            (unitless)
 !---------------------------------------------------------------------------------------!
 real (dp) :: z_l        ! For leaves
 real (dp) :: z_b        ! For branches
 real (dp) :: z_s        ! For stems
 real (dp) :: z_c        ! For coarse roots
 real (dp) :: z_f        ! For fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Growth rate of the tissues ([kg Xdm] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: G_M_l_M    ! Leaf meristem
 real (dp) :: G_M_l_X    ! Leaf structure
 real (dp) :: G_M_b_M    ! Branch meristem
 real (dp) :: G_M_b_X    ! Branch structure
 real (dp) :: G_M_s_M    ! Branch meristem
 real (dp) :: G_M_s_X    ! Branch structure
 real (dp) :: G_M_c_M    ! Branch meristem
 real (dp) :: G_M_c_X    ! Branch structure
 real (dp) :: G_M_f_M    ! Branch meristem
 real (dp) :: G_M_f_X    ! Branch structure
 !---------------------------------------------------------------------------------------!
 real (dp) :: c_a_l_a    ! Incremental specific leaf area               ([m]2 leaf [kg Xdm]-1) 
 real (dp) :: G_A_l      ! Leaf area growth rate                        ([m]2 [d]-1) 
 !---------------------------------------------------------------------------------------!
 ! Combined temperature and intrinsic leaf differentiation factor ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: k_dif_l    ! Leaves
 real (dp) :: k_dif_b    ! Branches
 real (dp) :: k_dif_s    ! Stems
 real (dp) :: k_dif_c    ! Coarse roots
 real (dp) :: k_dif_f    ! Fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Combined temperature and structural litter factor ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: k_l_X_lit  ! Leaves
 real (dp) :: k_b_X_lit  ! Branches
 real (dp) :: k_s_X_lit  ! Stems
 real (dp) :: k_c_X_lit  ! Coarse roots
 real (dp) :: k_f_X_lit  ! Fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 real (dp) :: k_A_l_lit   ! Combined temperature and foliage area litter factore ([d]-1)
 !---------------------------------------------------------------------------------------!
 ! Losses to differentiation of meristem             ([kg Xdm] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: L_M_l_M_dif ! Leaves
 real (dp) :: L_M_b_M_dif ! Branches
 real (dp) :: L_M_s_M_dif ! Stems
 real (dp) :: L_M_c_M_dif ! Coarse roots
 real (dp) :: L_M_f_M_dif ! Fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Losses to leaf structural litter                  ([kg Xdm] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: L_M_l_X_lit ! Leaves
 real (dp) :: L_M_b_X_lit ! Branches
 real (dp) :: L_M_s_X_lit ! Stems
 real (dp) :: L_M_c_X_lit ! Coarse roots
 real (dp) :: L_M_f_X_lit ! Fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 real (dp) :: L_A_l     ! Losses to leaf litter                        ([kg Xdm] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 ! Maintenance respiration                           ([kg C] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: R_l_X_m   ! In leaves
 real (dp) :: R_b_X_m   ! In branches
 real (dp) :: R_s_X_m   ! In stems
 real (dp) :: R_c_X_m   ! In coarse roots
 real (dp) :: R_f_X_m   ! In fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Growth respiration                                 ([kg C] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: R_l_X_G   ! In leaves
 real (dp) :: R_b_X_G   ! In branches
 real (dp) :: R_s_X_G   ! In stems
 real (dp) :: R_c_X_G   ! In coarse roots
 real (dp) :: R_f_X_G   ! In fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Maintenance coefficient                           ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: m_M_l_X   ! For leaves
 real (dp) :: m_M_f_X   ! For fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Utilisation of C for growth                        ([kg C] [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: U_C_l_G  ! In leaves
 real (dp) :: U_C_b_G  ! In branches
 real (dp) :: U_C_s_G  ! In stems
 real (dp) :: U_C_c_G  ! In coarse roots
 real (dp) :: U_C_f_G  ! In fine roots and mycorrhiza 
 !---------------------------------------------------------------------------------------!
 ! Utilisation of N for growth                        ([kg N] [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: U_N_l_G  ! In leaves
 real (dp) :: U_N_b_G  ! In branches
 real (dp) :: U_N_s_G  ! In stems
 real (dp) :: U_N_c_G  ! In coarse roots
 real (dp) :: U_N_f_G  ! In fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! C transport coefficient                    ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: c_tau_C_l_b ! Leaves       -> branches
 real (dp) :: c_tau_C_b_s ! Branches     -> stems
 real (dp) :: c_tau_C_s_c ! Stems        -> coarse roots
 real (dp) :: c_tau_C_c_f ! Coarse roots -> fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! N transport coefficient                    ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: c_tau_N_b_l ! Branches     -> Leaves
 real (dp) :: c_tau_N_s_b ! Stems        -> Branches
 real (dp) :: c_tau_N_c_s ! Coarse roots -> Stems
 real (dp) :: c_tau_N_f_c ! Fine roots   -> coarse roots
 !---------------------------------------------------------------------------------------!
 ! C transport conductances ([kg X dm] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: g_C_l_b     ! Leaves       -> branches
 real (dp) :: g_C_b_s     ! Branches     -> stems
 real (dp) :: g_C_s_c     ! Stems        -> coarse roots
 real (dp) :: g_C_c_f     ! Coarse roots -> fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! N transport conductances ([kg Xdm] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: g_N_b_l     ! Branches     -> leaves
 real (dp) :: g_N_s_b     ! Stems        -> branches
 real (dp) :: g_N_c_s     ! Coarse roots -> stems
 real (dp) :: g_N_f_c     ! Fine roots   -> leaves
 !---------------------------------------------------------------------------------------!
 ! C transport flux                                  ([kg C] [stem]-1 [d]-1) 
 !---------------------------------------------------------------------------------------!
 real (dp) :: T_C_l_b   ! Leaves       -> Branches
 real (dp) :: T_C_b_s   ! Branches     -> Stems
 real (dp) :: T_C_s_c   ! Stems        -> Coarse roots
 real (dp) :: T_C_c_f   ! Coarse roots -> fine roots
 !---------------------------------------------------------------------------------------!
 ! N transport flux                             ([kg N] [stem]-1 [d]-1) 
 !---------------------------------------------------------------------------------------!
 real (dp) :: T_N_b_l   ! Branches     -> leaves
 real (dp) :: T_N_s_b   ! Stems        -> branches
 real (dp) :: T_N_c_s   ! Coarse roots -> stems
 real (dp) :: T_N_f_c   ! Fine roots   -> coarse roots
 !---------------------------------------------------------------------------------------!
 ! Timestep increments for leaves
 !---------------------------------------------------------------------------------------!
 real (dp) :: dA_l      ! Change in leaf surface area
 real (dp) :: dM_l_M    ! Change in mass of meristem
 real (dp) :: dM_l_X    ! Change in mass of structural drymass
 real (dp) :: dM_l_C    ! Change in mass of C
 real (dp) :: dM_l_N    ! Change in mass of N
 !---------------------------------------------------------------------------------------!
 ! Timestep increments for branches
 !---------------------------------------------------------------------------------------!
 real (dp) :: dM_b_M    ! Change in mass of meristem
 real (dp) :: dM_b_X    ! Change in mass of structural drymass
 real (dp) :: dM_b_C    ! Change in mass of C
 real (dp) :: dM_b_N    ! Change in mass of N
 !---------------------------------------------------------------------------------------!
 ! Timestep increments for stems
 !---------------------------------------------------------------------------------------!
 real (dp) :: dM_s_M    ! Change in mass of meristem
 real (dp) :: dM_s_X    ! Change in mass of structural drymass
 real (dp) :: dM_s_C    ! Change in mass of C
 real (dp) :: dM_s_N    ! Change in mass of N
 !---------------------------------------------------------------------------------------!
 ! Timestep increments for coarse roots
 !---------------------------------------------------------------------------------------!
 real (dp) :: dM_c_M    ! Change in mass of meristem
 real (dp) :: dM_c_X    ! Change in mass of structural drymass
 real (dp) :: dM_c_C    ! Change in mass of C
 real (dp) :: dM_c_N    ! Change in mass of N
 !---------------------------------------------------------------------------------------!
 ! Timestep increments for fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 real (dp) :: dA_f      ! Change in leaf surface area
 real (dp) :: dM_f_M    ! Change in mass of meristem
 real (dp) :: dM_f_X    ! Change in mass of structural drymass
 real (dp) :: dM_f_C    ! Change in mass of C
 real (dp) :: dM_f_N    ! Change in mass of N
 !---------------------------------------------------------------------------------------!
 ! Declare tissue specific concentration
 !---------------------------------------------------------------------------------------!
 real (dp) :: C_l      ! C concentration in leaves         ([kg C] [kg dm]-1)
 real (dp) :: C_b      ! C concentration in branches       ([kg C] [kg dm]-1)
 real (dp) :: C_s      ! C concentration in stems          ([kg C] [kg dm]-1)
 real (dp) :: C_c      ! C concentration in coarse roots   ([kg C] [kg dm]-1)
 real (dp) :: C_f      ! C concentration in fine roots     ([kg C] [kg dm]-1)
 real (dp) :: N_l      ! C concentration in leaves         ([kg N] [kg dm]-1)
 real (dp) :: N_b      ! C concentration in branches       ([kg N] [kg dm]-1)
 real (dp) :: N_s      ! C concentration in stems          ([kg N] [kg dm]-1)
 real (dp) :: N_c      ! C concentration in coarse roots   ([kg N] [kg dm]-1)
 real (dp) :: N_f      ! C concentration in fine roots     ([kg N] [kg dm]-1)
 real (dp) :: N_l_tot  ! Total foliage N concentration     ([kg N])
 !---------------------------------------------------------------------------------------!
 ! Maintenance coefficients ([kg substrate C] [m]-2 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: m_A_b    ! Branches
 real (dp) :: m_A_s    ! Stems
 real (dp) :: m_A_c    ! Coarse roots
 !---------------------------------------------------------------------------------------!
 ! Uptake of nitrogen
 !---------------------------------------------------------------------------------------!
 real (dp) :: U_N      ! Uptake of N from soil        ([kg N] [stem] [d]-1)
 real (dp) :: U_N_amm  ! Uptake of ammonium           ()
 real (dp) :: U_N_nit  ! Uptake of nitrate            ()
 real (dp) :: c_U_N    ! Nitrate uptake factor        (unitless)
 real (dp) :: R_U_N    ! Respiratory cost of N uptake ()
 !---------------------------------------------------------------------------------------!
 ! Root activity variable    ([m]2 [kg X dm]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp) :: sigma_N
 
 !---------------------------------------------------------------------------------------!
end module TTR_variables
!========================================================================================!

!========================================================================================!
! This module provides the parameters for the simulations run by the TTR model as 
! implemented in TTR.f90
!----------------------------------------------------------------------------------------!
module TTR_parameters
!----------------------------------------------------------------------------------------!
 implicit none 
 
 ! Setup
 !---------------------------------------------------------------------------------------!
 integer, parameter :: dp = selected_real_kind (15)

 !---------------------------------------------------------------------------------------!
 ! Declare parameters
 !---------------------------------------------------------------------------------------!
 ! C content of meristem ([kg C] [kg X dm]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: f_C_l_M = 0.4    ! Leaves
 real (dp), parameter :: f_C_b_M = 0.4    ! Branches
 real (dp), parameter :: f_C_s_M = 0.4    ! Stems
 real (dp), parameter :: f_C_c_M = 0.4    ! Coarse roots
 real (dp), parameter :: f_C_f_M = 0.4    ! Fine   roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! C content of structure ([kg C] [kg X dm]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: f_C_l_X = 0.4    ! Leaves
 real (dp), parameter :: f_C_b_X = 0.4    ! Branches
 real (dp), parameter :: f_C_s_X = 0.4    ! Stems
 real (dp), parameter :: f_C_c_X = 0.4    ! Coarse roots
 real (dp), parameter :: f_C_f_X = 0.4    ! Fine   roots and mycorrhiza 
 !---------------------------------------------------------------------------------------!
 ! N content of meristems ([kg N] [kg X dm]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: f_N_l_M = 0.025  ! Leaves
 real (dp), parameter :: f_N_b_M = 0.025  ! Branches
 real (dp), parameter :: f_N_s_M = 0.025  ! Stems
 real (dp), parameter :: f_N_c_M = 0.025  ! Coarse roots
 real (dp), parameter :: f_N_f_M = 0.025  ! Fine   roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! N content of structure ([kg N] [kg X dm]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: f_N_l_X = 0.025  ! Leaves
 real (dp), parameter :: f_N_b_X = 0.025  ! Branches
 real (dp), parameter :: f_N_s_X = 0.025  ! Stems
 real (dp), parameter :: f_N_c_X = 0.025  ! Coarse roots
 real (dp), parameter :: f_N_f_X = 0.025  ! Fine   roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: tau       = 0.001  ! Leaf CO2 conductance          ([m]-1 [s]-1)
 real (dp), parameter :: N_l_tot_s = 0.040  ! Effect of N content on photosynthesis ([kg N] [kg dm]-1) 
 real (dp), parameter :: alpha_m   = 1.0d-6 ! Max. leaf photosynthetic efficiency ([kg CO2] [J]-1)
 real (dp), parameter :: beta      = 0.2d-6 ! Photorespiration parameter    ([kg CO2] [m]-2 [s]-1)
 real (dp), parameter :: k         = 0.5    ! Canopy extinction coef        ([m]2 ground [m]-2 leaf)
 real (dp), parameter :: shi_leaf  = 0.03   ! Leaf transmission coef        (unitless)
 real (dp), parameter :: theta     = 0.95   ! Leaf photosynthesis parameter (unitless)
 !---------------------------------------------------------------------------------------!
 ! Activity parameter of meristems    ([C]-1 [N]-1 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: k_l_M20   = 200.0  ! Leaves
 real (dp), parameter :: k_b_M20   = 200.0  ! Branches
 real (dp), parameter :: k_s_M20   = 200.0  ! Stems
 real (dp), parameter :: k_c_M20   = 200.0  ! Coarse roots
 real (dp), parameter :: k_f_M20   = 200.0  ! Fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Constant determining potential meristem size ([kg dm] [m]-2 [C]-1 [N]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: C_l_M_pot =  20.0  ! Leaves
 real (dp), parameter :: C_b_M_pot =  20.0  ! Branches
 real (dp), parameter :: C_s_M_pot =  20.0  ! Stems
 real (dp), parameter :: C_c_M_pot =  20.0  ! Coarse roots
 real (dp), parameter :: C_f_M_pot =  20.0  ! Fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: c_a_l_amx =   5.0  ! Max value of incremental SPA ([m]2 [kg Xdm]-1)
 real (dp), parameter :: c_a_l_a_C =   2.5  ! Specific leaf area (SPA) parameter ([C]-1)
 !---------------------------------------------------------------------------------------!
 ! Intrinsic differentiation parameter of meristem ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: k_dif_l20 =   0.0  ! Leaves
 real (dp), parameter :: k_dif_b20 =   0.0  ! Branches
 real (dp), parameter :: k_dif_s20 =   0.0  ! Stems
 real (dp), parameter :: k_dif_c20 =   0.0  ! Coarse roots
 real (dp), parameter :: k_dif_f20 =   0.0  ! Fine roots
 !---------------------------------------------------------------------------------------!
 ! Litter rate parameters of structure ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: k_l_X_lit20 = 0.0007 ! Leaves
 real (dp), parameter :: k_b_X_lit20 = 0.0007 ! Branches
 real (dp), parameter :: k_s_X_lit20 = 0.0007 ! Stems
 real (dp), parameter :: k_c_X_lit20 = 0.0007 ! Coarse roots
 real (dp), parameter :: k_f_X_lit20 = 0.0020 ! Fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: k_A_l_lit20 = 0.0007 ! Litter rate of foliage area ([d]-1)
 !---------------------------------------------------------------------------------------!
 ! Conversion coefficients for synthesis of meristem ([kg C] in dm [kg substrate ultilised]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: Y_l_M = 0.75  ! In leaves
 real (dp), parameter :: Y_b_M = 0.75  ! In branches
 real (dp), parameter :: Y_s_M = 0.75  ! In stems
 real (dp), parameter :: Y_c_M = 0.75  ! In coarse roots
 real (dp), parameter :: Y_f_M = 0.75  ! In fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Conversion coefficients for synthesis of structure ([kg C] in dm [kg substrate ultilised]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: Y_l_X = 0.75  ! In leaves
 real (dp), parameter :: Y_b_X = 0.75  ! In branches 
 real (dp), parameter :: Y_s_X = 0.75  ! In stems
 real (dp), parameter :: Y_c_X = 0.75  ! In coarse roots
 real (dp), parameter :: Y_f_X = 0.75  ! In fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Michaelis-Menten constant for maintenance ([C])
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: K_m_l_C   = 0.01   ! Leaves
 real (dp), parameter :: K_m_b_C   = 0.01   ! Branches
 real (dp), parameter :: K_m_s_C   = 0.01   ! Stems
 real (dp), parameter :: K_m_c_C   = 0.01   ! Coarse roots
 real (dp), parameter :: K_m_f_C   = 0.01   ! Fine roots and mycorrhiza
 !---------------------------------------------------------------------------------------!
 ! Maintenance coefficient                   ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: m_M_l_X20 = 0.001  ! For leaves
 real (dp), parameter :: m_M_f_X20 = 0.001  ! For foliage
 !---------------------------------------------------------------------------------------!
 ! Surface area constants                    ([m]2 [kg Xdm]-e_A)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: c_A_b     = 1.0    ! For branches
 real (dp), parameter :: c_A_s     = 1.0    ! For stems
 real (dp), parameter :: c_A_c     = 1.0    ! For coarse roots
 !---------------------------------------------------------------------------------------!
 ! Surface area constants                    (unitless)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: q_A_b     = 0.66667  
 real (dp), parameter :: q_A_s     = 0.66667  
 real (dp), parameter :: q_A_c     = 0.66667  
 !---------------------------------------------------------------------------------------!
 ! C and N transport coefficient             ([d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: c_tau_C_l_b20 = 50.0 ! Leaves       -> branches
 real (dp), parameter :: c_tau_C_b_s20 = 50.0 ! Branches     -> stems
 real (dp), parameter :: c_tau_C_s_c20 = 50.0 ! Stems        -> coarse roots
 real (dp), parameter :: c_tau_C_c_f20 = 50.0 ! Coarse roots -> fine   roots 
 real (dp), parameter :: c_tau_N_f_c20 =  2.0 ! Fine   roots -> coarse roots
 real (dp), parameter :: c_tau_N_c_s20 =  2.0 ! Coarse roots -> stems
 real (dp), parameter :: c_tau_N_s_b20 =  2.0 ! Stems        -> branches
 real (dp), parameter :: c_tau_N_b_l20 =  2.0 ! Branches     -> leaves
 !---------------------------------------------------------------------------------------!
 ! Maintenance coefficient ([kg substrate C] [m]-2 [d]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: m_A_b20 = 0.0005 ! Branches
 real (dp), parameter :: m_A_s20 = 0.0005 ! Stems
 real (dp), parameter :: m_A_c20 = 0.0005 ! Coarse roots
 !---------------------------------------------------------------------------------------!
 ! Nitrogen availability constants (unitless)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: c_U_N10 = 0.5 ! At 10 degC
 real (dp), parameter :: c_U_N20 = 1.0 ! At 20 degC
 !---------------------------------------------------------------------------------------!
 ! Respiratory cost of N uptake from soil ammonium and nitrate pools ([kg C] [kg N]-1)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: c_U_N_amm = 0.4 ! For ammonium
 real (dp), parameter :: c_U_N_nit = 0.5 ! For nitrate
 !---------------------------------------------------------------------------------------!
 ! Root activity constant at 20 degC 
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: sigma_N20 = 10.0  ! ([m]2 [kg X dm]-1 [d]-1)
 real (dp), parameter :: J_N_U_N   = 0.005 ! ([N])
 real (dp), parameter :: K_C_U_N   = 0.05  ! ([C])
 !---------------------------------------------------------------------------------------!
end module TTR_parameters
!========================================================================================!