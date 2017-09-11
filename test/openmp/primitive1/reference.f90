PROGRAM test_primitive
 INTEGER :: i

!$claw acc parallel
 DO i = 1 , 10 , 1
  PRINT * , i
 END DO
!$claw acc end parallel
!$omp do
 DO i = 1 , 10 , 1
  PRINT * , i
 END DO
!$omp end do
END PROGRAM test_primitive

