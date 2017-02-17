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
 real (dp) :: M_l_C ! Foliage C substrate          ([kg C] [stem]-1)
 real (dp) :: M_l_M ! Foliage meristem             ([kg dm] [stem])
 real (dp) :: M_l_N ! Foliage N substrate          ([kg N] [stem]-1) 
 real (dp) :: M_l_X ! Foliage structure            ([kg dm] [stem]-1)
 real (dp) :: L     ! Foliage area of plot         ([m]2)
 real (dp) :: A_l   ! Foliage surface area         ([m]2 [stem]-1)
 !---------------------------------------------------------------------------------------!
 ! Branches (subscript, b)
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_b_C ! Branch C substrate           ([kg C] [stem]-1)
 real (dp) :: M_b_M ! Branch meristem              ([kg dm] [stem])
 real (dp) :: M_b_N ! Branch N substrate           ([kg N] [stem]-1)
 real (dp) :: M_b_X ! Branch structure             ([kg dm] [stem]-1)
 real (dp) :: A_b   ! Surface area of branches     ([m]2)
 !---------------------------------------------------------------------------------------!
 ! Stems (subscript, s)
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_s_C ! Stem C substrate             ([kg C] [stem]-1)
 real (dp) :: M_s_M ! Stem meristem                ([kg dm] [stem])
 real (dp) :: M_s_N ! Stem N substrate             ([kg N] [stem]-1)
 real (dp) :: M_s_X ! Stem structure               ([kg dm] [stem]-1)
 real (dp) :: A_s   ! Surface area of the stem     ([m]2 [stem]-1)
 !---------------------------------------------------------------------------------------!
 ! Coarse roots (subscript, c)
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_c_C ! Coarse roots C substrate     ([kg C] [stem]-1)
 real (dp) :: M_c_M ! Coarse roots meristem        ([kg dm] [stem])
 real (dp) :: M_c_N ! Coarse roots N substrate     ([kg N] [stem]-1)
 real (dp) :: M_c_X ! Coarse roots structure       ([kg dm] [stem]-1)
 real (dp) :: A_c   ! Surface area of coarse roots ([m]2 [stem]-1)
 !---------------------------------------------------------------------------------------!
 ! Fine roots and mycorrhiza (subscript, f)
 !---------------------------------------------------------------------------------------!
 real (dp) :: M_f_C ! Fine roots C substrate       ([kg C] [stem]-1)
 real (dp) :: M_f_M ! Fine roots meristem          ([kg dm] [stem])
 real (dp) :: M_f_N ! Fine roots N substrate       ([kg N] [stem]-1)
 real (dp) :: M_f_X ! Fine roots structure         ([kg dm] [stem]-1)
 !---------------------------------------------------------------------------------------!
 
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
 real (dp) :: f_T_air   ! Air  temperature factor         (unitless)
 real (dp) :: f_T_soil  ! Soil temperature factor         (unitless)
 !---------------------------------------------------------------------------------------!

 !---------------------------------------------------------------------------------------!
 ! Declare spatio-temporal variables
 !---------------------------------------------------------------------------------------!
 integer :: t                      ! t                                         ([d])
 integer :: days                   ! Number of days in simulation
 integer :: tstps                  ! Number of timesteps in simulation         ([d])
 real (dp) :: n_stems              ! Number of stems     in simulation         (unitless)
 real (dp) :: dt                   ! Fraction of day of timestep  
 integer :: years                  ! Number of years in simulation             ([yr])
 integer :: stps_per_day           ! Number of timesteps per day in simulation ([d]-1)
 !---------------------------------------------------------------------------------------!        
 !---------------------------------------------------------------------------------------!

 !---------------------------------------------------------------------------------------!
 ! Declare variables
 !---------------------------------------------------------------------------------------!
 real (dp) :: I          ! Incident radiative flux                 ([J] [m-]2)
 real (dp) :: I_leaf     ! Incident radiative flux on leaf         ([J] [m]-2)
 real (dp) :: P_max20    ! Maximal rate of photosynthesis at 20 degC ([kg CO2] [m]2 leaf [s]-1)
 real (dp) :: P_max      ! Light-saturate rate of photosynthesis   ([kg CO2] [m]2 leaf [s]-1)
 real (dp) :: alpha      ! Leaf photosynthetic efficiency          ([kg CO2] [J]-1)
 real (dp) :: P_l        ! Leaf-level gross photosynthetic rate         ([kg CO2] [m]-2 leaf [s]-1)
 real (dp) :: P_c        ! Canopy-level gross photosynthetic reate      ([kg CO2] [m]-2 ground [s]-1)
 real (dp) :: P_carb     ! Canopy gross photosynthetic rate             ([kg CO2] [stem]-1 [d]-1)
 !---------------------------------------------------------------------------------------!

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
 ! Parameter activating Liebig's law of the minimum
 !---------------------------------------------------------------------------------------!
 logical:: LIEBIG 
 !---------------------------------------------------------------------------------------!
 
 !---------------------------------------------------------------------------------------!
end module TTR_variables
!========================================================================================!