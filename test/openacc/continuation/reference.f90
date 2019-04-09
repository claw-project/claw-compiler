PROGRAM openacc_cont

!$ACC parallel
!$ACC end parallel
!$acc data  present ( pti,pdp,pclc,pwv,psw,pqlwc,pqiwc,pduco2,pduo3  ) &
!$acc present ( paeq1,paeq2,paeq3,paeq4,paeq5,psmu0,palso      )  present ( &
!$acc palth,pskyview,pfcor                           )  present ( papre &
!$acc )  present ( pflt,pfls,pflt_s,pfls_s,pflsdir,pfltd,pfltu    )  present &
!$acc ( pflsd,pflsu,pflsp,pflpar,pflsu_par,pflsd_par   )  present ( pflsp_par &
!$acc )  present ( pti_dp,pdp_dp,pclc_dp,pwv_dp,psw_dp,pqlwc_dp   )  present &
!$acc ( pqiwc_dp,pduco2_dp,pduo3_dp,paeq1_dp,paeq2_dp  )  present ( &
!$acc paeq3_dp,paeq4_dp,paeq5_dp,psmu0_dp,palth_dp   )  present ( &
!$acc palso_dp,pskyview_dp,pfcor_dp,papre_dp,pflt_dp )  present ( &
!$acc pfls_dp,pflt_s_dp,pfls_s_dp,pflsdir_dp         )  present ( &
!$acc pfltd_dp,pfltu_dp,pflsd_dp,pflsu_dp,pflsp_dp   )  present ( &
!$acc pflpar_dp,pflsu_par_dp,pflsd_par_dp            )  present ( &
!$acc pflsp_par_dp                                   )
!$ACC end data
!$acc data  present(var1, var2)  present(var3, var4)  create(var5, &
!$acc varacc)  if (lzacc)
!$ACC end data
!$acc data  present ( aaaa,bbbb,ddd                               ) &
!$acc present ( zzzzz,yyyy,f,l_ggg                          )  create  ( &
!$acc datri_vec,y_vec,agdd_vec,ggg_vec,hhhh_gvec  )  create  ( &
!$acc gdbcjd_vec,dgdvbs_vec,dummyy                )  if (lxxxx)
!$ACC end data
!$acc data  coypin  ( shdhsahd, djsakj, asdsdsa, asdasdd, sdjsa, djahdj, &
!$acc dsajhdjk)  present ( qweqwq, asdsads, addsa,ppkmi, sss, dfjassaa, &
!$acc sdsdfsfssadaa )  present ( uikj,   fdhjkd, ujnh,   lokm,  dfds,ztzrtt, &
!$acc asjdsadsddhjdh )  copyin  ( dfdjfj, dksflkdsfl, sdjsadhfjds )
!$ACC end data
!$acc data if(lzacc)    present (ps,p0,pp,dp0,t,qv,qc,qrs,rho0)  create  (lzmask)
!$ACC end data
END PROGRAM openacc_cont

