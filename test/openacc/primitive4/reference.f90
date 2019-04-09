PROGRAM openacc_continuation
 INTEGER :: i
 REAL :: a ( 1 : 100 )
 REAL :: bvariable ( 1 : 100 )
 REAL :: cvariable ( 1 : 100 )
 REAL :: dvariable ( 1 : 100 )
 REAL :: evariable ( 1 : 100 )
 REAL :: fvariable ( 1 : 100 )
 REAL :: gvariable ( 1 : 100 )
 REAL :: hvariable ( 1 : 100 )
 REAL :: ivariable ( 1 : 100 )
 REAL :: jvariable ( 1 : 100 )
 REAL :: kvariable ( 1 : 100 )
 REAL :: lvariable ( 1 : 100 )

!$acc data copyin(a)  copyin(bvariable, cvariable, dvariable, evariable, &
!$acc fvariable)  copyin(gvariable, hvariable, ivariable, jvariable) &
!$acc copyin(kvariable, lvariable)
!$acc parallel
!$acc loop gang vector
 DO i = 1 , 100 , 1
  a ( i ) = bvariable ( i ) * 2.0
 END DO
!$acc end parallel
!$acc end data
END PROGRAM openacc_continuation

