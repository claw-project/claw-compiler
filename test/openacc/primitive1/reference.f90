PROGRAM test_primitive
 INTEGER :: i

!$acc parallel
 DO i = 1 , 10 , 1
  PRINT * , i
 END DO
!$acc end parallel
!$claw omp do
 DO i = 1 , 10 , 1
  PRINT * , i
 END DO
!$claw omp end do
END PROGRAM test_primitive

