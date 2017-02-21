!========================================================================================!
subroutine output
!----------------------------------------------------------------------------------------!
 use TTR_parameters
 use TTR_variables
 !---------------------------------------------------------------------------------------!
 implicit none
 !---------------------------------------------------------------------------------------!
 
 !---------------------------------------------------------------------------------------!
 ! Formats
 !---------------------------------------------------------------------------------------!
 1000 format (i15, 20f15.8) ! pools.txt
 1001 format (i15, 11f15.8) ! growth.txt
 1002 format (i15, 10f15.8) ! loss.txt
 1003 format (i15, 10f15.8) ! utilisation.txt
 1004 format (i15, 10f15.8) ! respiration.txt
 1005 format (i15,  8f15.8) ! transport.txt
 1006 format (i15, 21f15.8) ! increments.txt
 1007 format (i15, 11f15.8) ! concentrations.txt
 1008 format (i15,  4f15.8) ! uptake.txt
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_POOLS) then 
   write (20, 1000) t, M_l_C, M_b_C, M_s_C, M_c_C, M_f_C,                                &
                       M_l_N, M_b_N, M_s_N, M_c_N, M_f_N,                                &
                       M_l_M, M_b_M, M_s_M, M_c_M, M_f_M,                                &
                       M_l_X, M_b_X, M_s_X, M_c_X, M_f_X
 end if
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_GROWTH) then  
   write (21, 1001) t, G_A_l, G_M_l_X, G_M_b_X, G_M_s_X, G_M_c_X, G_M_f_X,               &
                              G_M_l_M, G_M_b_M, G_M_s_M, G_M_c_M, G_M_f_M 
 end if
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_LOSS) then  
   write (22, 1002) t, L_M_l_M_dif, L_M_l_X_lit,                                         &
                       L_M_b_M_dif, L_M_b_X_lit,                                         &
                       L_M_s_M_dif, L_M_s_X_lit,                                         &
                       L_M_c_M_dif, L_M_c_X_lit,                                         &
                       L_M_f_M_dif, L_M_f_X_lit
 end if
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_UTILISATION) then  
   write (23, 1003) t, U_C_l_G, U_C_b_G, U_C_s_G, U_C_c_G, U_C_f_G,                      &
                       U_N_l_G, U_N_b_G, U_N_s_G, U_N_c_G, U_N_f_G
 end if
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_RESPIRATION) then  
   write (24, 1004) t, R_l_X_m, R_b_X_m, R_s_X_m, R_c_X_m, R_f_X_m,                      &
                       R_l_X_G, R_b_X_G, R_s_X_G, R_c_X_G, R_f_X_G  
 end if
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_TRANSPORT) then   
   write (25, 1005) t, T_C_l_b, T_C_b_s, T_C_s_c, T_C_c_f,                               &
                       T_N_f_c, T_N_c_s, T_N_s_b, T_N_b_l
 end if
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_INCREMENTS) then  
   write (26, 1006) t, dA_l, dM_l_M, dM_l_X, dM_l_C, dM_l_N,                             &
                             dM_b_M, dM_b_X, dM_b_C, dM_b_N,                             & 
                             dM_s_M, dM_s_X, dM_s_C, dM_s_N,                             &
                             dM_c_M, dM_c_X, dM_c_C, dM_c_N,                             &
                             dM_f_M, dM_f_X, dM_f_C, dM_f_N 
  end if
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_CONCENTRATIONS) then  
     write (27, 1007) t, C_l, C_b, C_s, C_c, C_f,                                        &
                         N_l, N_b, N_s, N_c, N_f, N_l_tot 
 end if
 
 !---------------------------------------------------------------------------------------!
 ! Write values to pools.txt output file
 !---------------------------------------------------------------------------------------!
 if (R_UPTAKE) then  
     write (28, 1008) t, P_carb, U_N, U_N_amm, U_N_nit   
 end if 
   
 ! Close output files
 !---------------------------------------------------------------------------------------!
 !close (20) ! Close pools.txt output file
 !close (21) ! Close growth.txt output file
 !close (22) ! Close loss.txt output file
 !close (23) ! Close utilisation.txt output file
 !close (24) ! Close respiration.txt output file
 !close (25) ! Close transport.txt output file
 !close (26) ! Close increments.txt output file
 !close (27) ! Close concentrations.txt output file
 !close (28) ! Close uptake.txt output file
 
!----------------------------------------------------------------------------------------!
end subroutine output
!========================================================================================!