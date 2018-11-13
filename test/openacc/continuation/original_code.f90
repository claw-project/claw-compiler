!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

PROGRAM openacc_cont
  !$claw remove
  PRINT*, 'dummy'
  !$claw end remove

  !$acc parallel

  !$acc end parallel

  !$acc data &
  !---- Argument arrays - intent(in)
  !$acc present ( pti,pdp,pclc,pwv,psw,pqlwc,pqiwc,pduco2,pduo3  ) &
  !$acc present ( paeq1,paeq2,paeq3,paeq4,paeq5,psmu0,palso      ) &
  !$acc present ( palth,pskyview,pfcor                           ) &
  !---- Argument arrays - intent(inout)
  !$acc present ( papre                                          ) &
  !---- Argument arrays - intent(out)
  !$acc present ( pflt,pfls,pflt_s,pfls_s,pflsdir,pfltd,pfltu    ) &
  !$acc present ( pflsd,pflsu,pflsp,pflpar,pflsu_par,pflsd_par   ) &
  !$acc present ( pflsp_par                                      ) &
  !---- Local automatic arrays
  !$acc present ( pti_dp,pdp_dp,pclc_dp,pwv_dp,psw_dp,pqlwc_dp   ) &
  !$acc present ( pqiwc_dp,pduco2_dp,pduo3_dp,paeq1_dp,paeq2_dp  ) &
  !$acc present ( paeq3_dp,paeq4_dp,paeq5_dp,psmu0_dp,palth_dp   ) &
  !$acc present ( palso_dp,pskyview_dp,pfcor_dp,papre_dp,pflt_dp ) &
  !$acc present ( pfls_dp,pflt_s_dp,pfls_s_dp,pflsdir_dp         ) &
  !$acc present ( pfltd_dp,pfltu_dp,pflsd_dp,pflsu_dp,pflsp_dp   ) &
  !$acc present ( pflpar_dp,pflsu_par_dp,pflsd_par_dp            ) &
  !$acc present ( pflsp_par_dp                                   )

  !$acc end data

  !$acc data &
  !---- Some comment 1
  !$acc present(var1, var2) &
  !---- Some comment 2
  !$acc present(var3, var4) &
  !---- Some comment 3
  !$acc create(var5, varacc) &
  !$acc if (lzacc)

  !$acc end data


  !$acc data &
  !---- Comment
  !$acc present ( aaaa,bbbb,ddd                               ) &
  !---- Comment
  !$acc present ( zzzzz,yyyy,f,l_ggg                          ) &
  !---- Comment
  !$acc create  ( datri_vec,y_vec,agdd_vec,ggg_vec,hhhh_gvec  ) &
  !$acc create  ( gdbcjd_vec,dgdvbs_vec,dummyy                ) &
  !$acc if (lxxxx)

  !$acc end data

  !$acc data &
  ! Comment line 1
  ! Comment line 2
  ! Comment line 3
  !!$acc coypin  ( fnodivc,rhqsond,rhtsond,rhvsond
  !$acc coypin  ( shdhsahd, djsakj, asdsdsa, asdasdd, sdjsa, djahdj, dsajhdjk) &
  !$acc present ( qweqwq, asdsads, addsa,ppkmi, sss, dfjassaa, sdsdfsfssadaa ) &
  !$acc present ( uikj,   fdhjkd, ujnh,   lokm,  dfds,ztzrtt, asjdsadsddhjdh ) &
  !$acc copyin  ( dfdjfj, dksflkdsfl, sdjsadhfjds ) ! both only of dimension (4)

  !$acc end data


  !$acc data if(lzacc)   &
  !$acc present (ps,p0,pp,dp0,t,qv,qc,qrs,rho0) &
  !$acc create  (lzmask) !XL_TODO make lzmask global

  !$acc end data

END PROGRAM openacc_cont
