PROGRAM openacc_continuation
 INTEGER :: i
 REAL :: a ( 1 : 100 )
 REAL :: b ( 1 : 100 )

!$acc data copyin(a) &
!$acc present(b)
!$acc parallel
!$acc loop gang vector
 DO i = 1 , 100 , 1
  a ( i ) = b ( i ) * 2.0
 END DO
!$acc end parallel
!$acc end data
END PROGRAM openacc_continuation

