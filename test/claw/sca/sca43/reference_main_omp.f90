PROGRAM test_abstraction1
 USE mo_column , ONLY: compute_point
 INTEGER , PARAMETER :: nproma = 20
 REAL :: q ( 1 : 20 )
 REAL :: t ( 1 : 20 )
 REAL :: w ( 1 : 20 )
 INTEGER :: i
 INTEGER :: k

 DO i = 1 , 20 , 1
  t ( i ) = 0.5 * i
 END DO
!$omp target data map(alloc:t)
!$omp target update to(t)
 q = compute_point ( t , nproma = nproma , w = w )
!$omp end target data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction1

