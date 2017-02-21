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
 use TTR_variables
 use TTR_parameters
 !---------------------------------------------------------------------------------------!

 !---------------------------------------------------------------------------------------!
 ! Initialise state variables and concentrations
 !---------------------------------------------------------------------------------------!
 ! Foliage (subscript, l)
 !---------------------------------------------------------------------------------------!
 A_l   = 0.0030000               ! Foliage area               ([m]2 [stem]-1)
 M_l_C = 0.0000400               ! Foliage C substrate        ([kg C] [stem]-1)
 M_l_M = 0.0001000               ! Foliage meristem           ([kg dm] [stem])
 M_l_N = 0.0000100               ! Foliage N substrate        ([kg N] [stem]-1)
 M_l_X = 0.0010000               ! Foliage structure          ([kg dm] [stem]-1)
 C_l   = M_l_C / (M_l_X + M_l_M) ! Foliage C concentration    ([kg C] [kg dm]-1)
 N_l   = M_l_N / (M_l_X + M_l_M) ! Foliage N concentration    ([kg N] [kg dm]-1)
 N_l_tot = (M_l_N + f_N_l_X * M_l_X + f_N_l_M * M_l_M) / (M_l_X + M_l_M) ! Total N concentration in foliage ([kg N] [kg Xdm]-1)
 !---------------------------------------------------------------------------------------!
 ! Branches (subscript, b)
 !---------------------------------------------------------------------------------------!
 M_b_C = 0.0000035               ! Branch C substrate         ([kg C] [stem]-1)
 M_b_M = 0.0001000               ! Branch meristem            ([kg dm] [stem])
 M_b_N = 0.0000015               ! Branch N substrate         ([kg N] [stem]-1)
 M_b_X = 0.0010000               ! Branch structure           ([kg dm] [stem]-1)
 C_b   = M_b_C / M_b_M           ! Branch C concentration     ([kg C] [kg dm]-1)
 N_b   = M_b_N / M_b_M           ! Branch N concentration     ([kg N] [kg dm]-1)
 !---------------------------------------------------------------------------------------!
 ! Stems (subscript, s)
 !---------------------------------------------------------------------------------------!
 M_s_C = 0.0000030               ! Stem C substrate           ([kg C] [stem]-1)
 M_s_M = 0.0001000               ! Stem meristem              ([kg dm] [stem])
 M_s_N = 0.0000020               ! Stem N substrate           ([kg N] [stem]-1)
 M_s_X = 0.0010000               ! Stem structure             ([kg dm] [stem]-1)
 C_s   = M_s_C / M_s_M           ! Stem C concentration       ([kg C] [kg dm]-1)
 N_s   = M_s_N / M_s_M           ! Stem N concentration       ([kg N] [kg dm]-1)
 !---------------------------------------------------------------------------------------!
 ! Coarse roots (subscript, c)
 !---------------------------------------------------------------------------------------!
 M_c_C = 0.0000025             ! Coarse roots C substrate     ([kg C] [stem]-1)
 M_c_M = 0.0001000             ! Coarse roots meristem        ([kg dm] [stem])
 M_c_N = 0.0000025             ! Coarse roots N substrate     ([kg N] [stem]-1)
 M_c_X = 0.0010000             ! Coarse roots structure       ([kg dm] [stem]-1)
 C_c = M_c_C / M_c_M           ! Coarse roots C concentration ([kg C] [kg dm]-1)
 N_c = M_c_N / M_c_M           ! Coarse roots N concentration ([kg N] [kg dm]-1)
 !---------------------------------------------------------------------------------------!
 ! Fine roots and mycorrhiza (subscript, f)
 !---------------------------------------------------------------------------------------!
 M_f_C = 0.0000200               ! Fine roots C substrate     ([kg C] [stem]-1)
 M_f_M = 0.0001000               ! Fine roots meristem        ([kg dm] [stem])
 M_f_N = 0.0000300               ! Fine roots N substrate     ([kg N] [stem]-1)
 M_f_X = 0.0010000               ! Fine roots structure       ([kg dm] [stem]-1)
 C_f   = M_f_C / (M_f_X + M_f_M) ! Fine roots C concentration ([kg C] [kg dm]-1)
 N_f   = M_f_N / (M_f_X + M_f_M) ! Fine roots N concentration ([kg N] [kg dm]-1)
 
 !---------------------------------------------------------------------------------------!
 ! Initialise environmental variables
 !---------------------------------------------------------------------------------------!
 C_CO2  =     400.0     ! CO2 concentration               ([ppmv])
 J      = 5000000.0     ! Daily radiation recept          ([J] [m]-2 [d]-1)
 N_amm  =       0.00075 ! Soil mineral ammonium level     ([kg N] [m]-2)
 N_nit  =       0.00025 ! Soil mineral nitrate level      ([kg N] [m]-2)
 p_atm  = 101325.0      ! Mean daily atmospheric pressure ([Pa])
 T_air  =     14.0      ! Mean daily air temperature      ([degC])
 T_soil =     14.0      ! mean daily soil temperature     ([degC])
 h      = 50400         ! Day length                      ([s] [d]-1)
 !---------------------------------------------------------------------------------------!

 !---------------------------------------------------------------------------------------!
 ! Open driver file to read setup variables
 !---------------------------------------------------------------------------------------!
 open (10, file = 'driver.txt', status = 'old', action = 'read')
 do t = 1, 5 ! Read the header and skip it
   read (10, *)
 end do
 t = 0 ! Reset t to zero for initial output files values
 read (10, '(i6)')   years
 read (10, '(i6)')   stps_per_day
 read (10, '(f6.1)') n_stems 
 read (10, '(l6)')   LIEBIG 
 read (10, '(l6)')   REPORTING
 read (10, '(l6)')   R_POOLS
 read (10, '(l6)')   R_GROWTH
 read (10, '(l6)')   R_LOSS
 read (10, '(l6)')   R_UTILISATION
 read (10, '(l6)')   R_RESPIRATION
 read (10, '(l6)')   R_TRANSPORT
 read (10, '(l6)')   R_INCREMENTS
 read (10, '(l6)')   R_CONCENTRATIONS
 read (10, '(l6)')   R_UPTAKE
 write (*, *) 
 write (*, '(a36)') '             TTR Model             '
 write (*, '(a36)') ' programmed by Tim Tito Rademacher '
 write (*, *) 
 write (*, '(a36)') 'Simulation parameters:             '
 write (*, '(a36)') '-----------------------------------'
 write (*, 997)  years
 write (*, 998)  stps_per_day
 write (*, 999)  n_stems 
 997 format ('Years in simulation: ',i5,  ' years')
 998 format ('Time steps per day:  ',i5,  ' d-1')
 999 format ('Stems density:       ',f5.1,' stems m-2')
 
 !---------------------------------------------------------------------------------------!
 ! Calculate number of time steps
 !---------------------------------------------------------------------------------------!
 tstps = floor (years * days_per_year) * stps_per_day
 dt    = 1.0 / real (stps_per_day)
 !write (*, *) tstps, dt
 
 !---------------------------------------------------------------------------------------!
 ! Open outputs files and save parameters for simulation
 !---------------------------------------------------------------------------------------!
 open  (20, file = 'tmp/pools.txt',          status = 'unknown', action = 'write') 
 open  (21, file = 'tmp/growth.txt',         status = 'unknown', action = 'write')
 open  (22, file = 'tmp/loss.txt',           status = 'unknown', action = 'write')
 open  (23, file = 'tmp/utilisation.txt',    status = 'unknown', action = 'write')
 open  (24, file = 'tmp/respiration.txt',    status = 'unknown', action = 'write')
 open  (25, file = 'tmp/transport.txt',      status = 'unknown', action = 'write')
 open  (26, file = 'tmp/increments.txt',     status = 'unknown', action = 'write')
 open  (27, file = 'tmp/concentrations.txt', status = 'unknown', action = 'write')
 open  (28, file = 'tmp/uptake.txt',         status = 'unknown', action = 'write')

 !---------------------------------------------------------------------------------------!
 ! Write headers to output files
 !---------------------------------------------------------------------------------------!
 write (20, '(21a15)') 'time', 'M_l_C','M_b_C','M_s_C','M_c_C','M_f_C',                  &
                               'M_l_N','M_b_N','M_s_N','M_c_N','M_f_N',                  &
                               'M_l_M','M_b_M','M_s_M','M_c_M','M_f_M',                  &
                               'M_l_X','M_b_X','M_s_X','M_c_X','M_f_X'
 write (21, '(12a15)') 'time','G_A_l','G_M_l_X','G_M_b_X','G_M_s_X','G_M_c_X','G_M_f_X', &
                              'G_M_l_M','G_M_b_M','G_M_s_M','G_M_c_M','G_M_f_M'
 write (22, '(11a15)') 'time','L_M_l_M_dif','L_M_l_X_lit','L_M_b_M_dif','L_M_b_X_lit',   &
                              'L_M_s_M_dif','L_M_s_X_lit','L_M_c_M_dif','L_M_c_X_lit',   &
                              'L_M_f_M_dif','L_M_f_X_lit'
 write (23, '(11a15)') 'time','U_C_l_G','U_C_b_G','U_C_s_G','U_C_c_G','U_C_f_G',         &
                              'U_N_l_G','U_N_b_G','U_N_s_G','U_N_c_G','U_N_f_G'
 write (24, '(11a15)') 'time','R_l_X_m','R_b_X_m','R_s_X_m','R_c_X_m','R_f_X_m',         &
                              'R_l_X_G','R_b_X_G','R_s_X_G','R_c_X_G','R_f_X_G'                            
 write (25, '(9a15)') 'time', 'T_C_l_b', 'T_C_b_s', 'T_C_s_c', 'T_C_c_f',                &
                              'T_N_b_l', 'T_N_s_b', 'T_N_c_s', 'T_N_f_c'
 write (26, '(22a15)') 'time', 'dA_l',  'dM_l_M','dM_l_X','dM_l_C','dM_l_N','dM_b_M',    &
                               'dM_b_X','dM_b_C','dM_b_N','dM_s_M','dM_s_X','dM_s_C',    &
                               'dM_s_N','dM_c_M','dM_c_X','dM_c_C','dM_c_N','dM_f_M',    &
                               'dM_f_X','dM_f_C','dM_f_N'           
 write (27, '(12a15)') 'time', 'C_l', 'C_b', 'C_s', 'C_c', 'C_f',                        &
                               'N_l', 'N_b', 'N_s', 'N_c', 'N_f', 'N_l_tot'    
 write (28, '(5a15)') 'time', 'P_carb', 'U_N', 'U_N_amm', 'U_N_nit'

 !---------------------------------------------------------------------------------------!
 ! Write initial values
 !---------------------------------------------------------------------------------------!
 call output
 
 !---------------------------------------------------------------------------------------!
 ! Open and write environmental output file
 !---------------------------------------------------------------------------------------!
 open  (31, file = 'tmp/environment.txt',    status = 'unknown', action = 'write')
 write (31, '(8a14)') 'C_CO2 [ppm]','T_air [degC]','T_soil [degC]','p_atm [Pa]',             &
                      'J [J m-2 d-1]','h [s]','N_amm [kg m-2]','N_nit [kg m-2]'
 write (31, '(5f14.5, i14, 2f14.5)') C_CO2, T_air, T_soil, p_atm, J, h, N_amm, N_nit
 close (31) ! Close environment.txt output file
 
 !---------------------------------------------------------------------------------------!
 ! Loop over timesteps
 !---------------------------------------------------------------------------------------!
 timestep : do t = 1, tstps
   
   !-------------------------------------------------------------------------------------! 
   ! Calculate CO2 concentration in ambient air and temperature factors
   !-------------------------------------------------------------------------------------!
   C_CO2_air = (C_CO2 / 10d6) * (273.15 / T_air + 273.15) * (p_atm / 101325) * 1.9636
   f_T_air   = f_T (T_air)
   f_T_soil  = f_T (T_soil)
   
   !-------------------------------------------------------------------------------------!
   ! Call C uptake subroutine to determine the uptake of carbon
   !-------------------------------------------------------------------------------------!
   call C_uptake 
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 11a12)') '  ','L','I','P_max20','P_max','alpha',        &
   !                          'I_leaf','P_g1','P_g2','P_g','P_CO2','P_carb' 
   !write (*, '(a15, 11f12.5)') 'Photosynthesis ', L, I, P_max20, P_max, alpha, I_leaf,   &
   !                                               P_g1, P_g2, P_g, P_CO2, P_carb

   !-------------------------------------------------------------------------------------!
   ! N uptake by the fine roots and mycorrhiza
   !-------------------------------------------------------------------------------------!
   call N_uptake
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 3a12)')   '                ','U_N','U_N_nit','U_N_amm'
   !write (*, '(a15, 3f12.5)') 'Nitrogen uptake ', U_N , U_N_nit,  U_N_amm
             
   !-------------------------------------------------------------------------------------!
   ! Growth (G) of meristematic and structure dry matter for leaves
   !-------------------------------------------------------------------------------------!
   call growth 
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 11a12)')   ' ','G_A_l', 'G_M_l_M','G_M_l_X','G_M_b_M','G_M_b_X',    &
   !                            'G_M_s_M','G_M_s_X','G_M_c_M','G_M_c_X','G_M_f_M','G_M_f_X'
   !write (*, '(a15, 11f12.5)') 'Growth  ', G_A_l, G_M_l_M, G_M_l_X, G_M_b_M, G_M_b_X,      &
   !                             G_M_s_M, G_M_s_X, G_M_c_M, G_M_c_X, G_M_f_M, G_M_f_X
                                                     
   !-------------------------------------------------------------------------------------!
   ! Losses (L) to differentiation and to litter for leaves
   !-------------------------------------------------------------------------------------!
   call loss
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 10a12)') ' ', 'L_M_l_M_dif', 'L_M_l_X_lit', 'L_M_b_M_dif',& 
   !                               'L_M_b_X_lit', 'L_M_s_M_dif', 'L_M_s_X_lit',&
   !                               'L_M_c_M_dif', 'L_M_c_X_lit', 'L_M_f_M_dif',&
   !                               'L_M_f_X_lit'
   !write (*, '(a15, 10f12.5)') 'Losses to litter',L_M_l_M_dif, L_M_l_X_lit, L_M_b_M_dif,& 
   !                                               L_M_b_X_lit, L_M_s_M_dif, L_M_s_X_lit,&
   !                                               L_M_c_M_dif, L_M_c_X_lit, L_M_f_M_dif,&
   !                                               L_M_f_X_lit
                                                
   !-------------------------------------------------------------------------------------! 
   ! Utilisation (U) of C and N substrate
   !-------------------------------------------------------------------------------------!
   call utilisation
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 5a12)') ' ', 'U_C_l_G', 'U_C_b_G', 'U_C_s_G', 'U_C_c_G', 'U_C_f_G'
   !write (*, '(a15, 5f12.5)') 'C utilisation', U_C_l_G, U_C_b_G, U_C_s_G, U_C_c_G, U_C_f_G
   !write (*, *)
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 5a12)') ' ', 'U_N_l_G', 'U_N_b_G', 'U_N_s_G', 'U_N_c_G', 'U_N_f_G'
   !write (*, '(a15, 5f12.5)') 'N utilisation', U_N_l_G, U_N_b_G, U_N_s_G, U_N_c_G, U_N_f_G
                                                  
   !-------------------------------------------------------------------------------------!
   ! Respiration
   !-------------------------------------------------------------------------------------!
   call respiration
   !-------------------------------------------------------------------------------------!
   !write (*, '(3f12.8)') C_b, M_b_C, M_b_M
   !write (*, '(a15, 5a12)') ' ', 'R_l_X_m', 'R_b_X_m', 'R_s_X_m', 'R_c_X_m', 'R_f_X_m'
   !write (*, '(a15, 5f12.5)') 'Respiration ', R_l_X_m, R_b_X_m, R_s_X_m, R_c_X_m, R_f_X_m
   !write (*, *)
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 5a12)') ' ', 'R_l_X_G', 'R_b_X_G', 'R_s_X_G', 'R_c_X_G', 'R_f_X_G'
   !write (*, '(a15, 5f12.5)') 'Respiration ', R_l_X_G, R_b_X_G, R_s_X_G, R_c_X_G, R_f_X_G
                                                          
   !-------------------------------------------------------------------------------------!
   ! Transport fluxes (T) of C and N
   !-------------------------------------------------------------------------------------!
   call transport
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 4a12)')   '            ','T_C_l_b','T_C_b_s','T_C_s_c','T_C_c_f'
   !write (*, '(a15, 4f12.5)') 'C transport ', T_C_l_b,  T_C_b_s,  T_C_s_c,  T_C_c_f
   !write (*, *)
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 4a12)')   '            ','T_N_b_l','T_N_s_b','T_N_c_s','T_N_f_c'
   !write (*, '(a15, 4f12.5)') 'N transport ', T_N_b_l,  T_N_s_b,  T_N_c_s,  T_N_f_c
 
   !-------------------------------------------------------------------------------------!
   ! Calculate increments and update pools
   !-------------------------------------------------------------------------------------!
   call increments
   !-------------------------------------------------------------------------------------!
   !write (*, '(a15, 21a12 )') ' ', 'dA_l', 'dM_l_M', 'dM_l_X', 'dM_l_C', 'dM_l_N',      &
   !                           'dM_b_M', 'dM_b_X', 'dM_b_C', 'dM_b_N', 'dM_s_M', 'dM_s_X',&
   !                           'dM_s_C', 'dM_s_N', 'dM_c_M', 'dM_c_X', 'dM_c_C', 'dM_c_N',&
   !                           'dM_f_M', 'dM_f_X', 'dM_f_C', 'dM_f_N'
   !write (*, '(a15, 21f12.5)') 'Change ', dA_l, dM_l_M, dM_l_X, dM_l_C, dM_l_N, dM_b_M,  &
   !                             dM_b_X, dM_b_C, dM_b_N, dM_s_M, dM_s_X, dM_s_C, dM_s_N,  &
   !                             dM_c_M, dM_c_X, dM_c_C, dM_c_N, dM_f_M, dM_f_X, dM_f_C,  &
   !                             dM_f_N
   
   !-------------------------------------------------------------------------------------! 
   ! Write year to screen
   !-------------------------------------------------------------------------------------! 
   if (modulo (t, int (stps_per_day * days_per_year)) == 0) then
     write (*, '(a4, i3)') 'Year ', int (t * dt / days_per_year)
   end if
   
   !-------------------------------------------------------------------------------------!
   ! Write diagnostics and state variables
   !-------------------------------------------------------------------------------------! 
   write_output : if ((modulo (t, stps_per_day) == 0) .and. (REPORTING)) then
     call output
   else if ((modulo (t, int (stps_per_day * days_per_year)) == 0) .and.                  &
            (.not. REPORTING)) then
     call output
   end if write_output
                            
 !---------------------------------------------------------------------------------------! 
 end do timestep ! End of timestep loop

 ! Close output files
 !---------------------------------------------------------------------------------------!
 close (20) ! Close pools.txt output file
 close (21) ! Close growth.txt output file
 close (22) ! Close loss.txt output file
 close (23) ! Close utilisation.txt output file
 close (24) ! Close respiration.txt output file
 close (25) ! Close transport.txt output file
 close (26) ! Close increments.txt output file
 close (27) ! Close concentrations.txt output file
 close (28) ! Close uptake.txt output file
 
contains
 !=======================================================================================!
 function f_T (T_inp)
  !--------------------------------------------------------------------------------------!
  integer, parameter :: dp = selected_real_kind (15)
  real (dp) :: f_T ! Temperature function (unitless)
  real (dp), intent (in) :: T_inp        ! Input temperature - either air or soil ([degC])
  real (dp), parameter :: T_0   =  0.0d0 ! Temperature at which rate processes cease ([degC])
  real (dp), parameter :: T_ref = 20.0d0 ! Reference temperature at which f_T = 1 ([degC])
  real (dp), parameter :: T_max = 30.0d0 ! Temperature at which rate processes are maximum ([degC])
 
  f_T   = ((T_inp - T_0) * (2 * T_max - T_0 - T_inp)) /                                   &
          ((T_ref - T_0) * (2 * T_max - T_0 - T_ref)) 
  !--------------------------------------------------------------------------------------!
  return
  !--------------------------------------------------------------------------------------!
 end function f_T
 !=======================================================================================!
!----------------------------------------------------------------------------------------!
end program TTR_model
!========================================================================================!