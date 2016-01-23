PROGRAM openacc_cont

!$acc parallel
!$acc end parallel
!$acc  data  &
!$acc  present ( pti,pdp,pclc,pwv,psw,pqlwc,pqiwc,pduco2,pduo3  )  &
!$acc  present ( paeq1,paeq2,paeq3,paeq4,paeq5,psmu0,palso      )  &
!$acc  present ( palth,pskyview,pfcor                           )  &
!$acc  present ( papre                                          )  &
!$acc  present ( pflt,pfls,pflt_s,pfls_s,pflsdir,pfltd,pfltu    )  &
!$acc  present ( pflsd,pflsu,pflsp,pflpar,pflsu_par,pflsd_par   )  &
!$acc  present ( pflsp_par                                      )  &
!$acc  present ( pti_dp,pdp_dp,pclc_dp,pwv_dp,psw_dp,pqlwc_dp   )  &
!$acc  present ( pqiwc_dp,pduco2_dp,pduo3_dp,paeq1_dp,paeq2_dp  )  &
!$acc  present ( paeq3_dp,paeq4_dp,paeq5_dp,psmu0_dp,palth_dp   )  &
!$acc  present ( palso_dp,pskyview_dp,pfcor_dp,papre_dp,pflt_dp )  &
!$acc  present ( pfls_dp,pflt_s_dp,pfls_s_dp,pflsdir_dp         )  &
!$acc  present ( pfltd_dp,pfltu_dp,pflsd_dp,pflsu_dp,pflsp_dp   )  &
!$acc  present ( pflpar_dp,pflsu_par_dp,pflsd_par_dp            )  &
!$acc  present ( pflsp_par_dp                                   )
END PROGRAM openacc_cont

