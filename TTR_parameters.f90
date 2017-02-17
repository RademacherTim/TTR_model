!========================================================================================!
! This module provides the parameters for the simulations run by the TTR model as 
! implemented in TTR.f90
!----------------------------------------------------------------------------------------!
module TTR_parameters
!----------------------------------------------------------------------------------------!
 implicit none 
 
 !---------------------------------------------------------------------------------------!
 ! Setup
 !---------------------------------------------------------------------------------------!
 integer, parameter :: dp = selected_real_kind (15)
 real (dp), parameter :: days_per_year = 365.25
 
 !---------------------------------------------------------------------------------------!
 ! Declare parameters
 !---------------------------------------------------------------------------------------!
 ! Control parameter
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: epsilon = 0.0000001 ! Very small number
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
 real (dp), parameter :: K_c_U_N   = 0.05  ! ([C])
 !---------------------------------------------------------------------------------------!
 ! Threshold concentrations for growth to occur ! Fits from Berninger et al. (2000)
 !---------------------------------------------------------------------------------------!
 real (dp), parameter :: C_l_threshold = 0.01
 real (dp), parameter :: C_b_threshold = 0.01
 real (dp), parameter :: C_s_threshold = 0.01
 real (dp), parameter :: C_c_threshold = 0.008
 real (dp), parameter :: C_f_threshold = 0.008
 real (dp), parameter :: N_l_threshold = 0.04 
 real (dp), parameter :: N_b_threshold = 0.04
 real (dp), parameter :: N_s_threshold = 0.04
 real (dp), parameter :: N_c_threshold = 0.001
 real (dp), parameter :: N_f_threshold = 0.001
 !---------------------------------------------------------------------------------------!
end module TTR_parameters
!========================================================================================!