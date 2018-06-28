PROGRAM test_abstraction8
 USE mo_column_extra , ONLY: compute_one
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 20 , 1 : 60 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p

 nproma = 20
 nz = 60
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
  t ( p , 1 ) = 0.0
 END DO
!$omp target data map(alloc:q(:,:),t(:,:))
!$omp target update to(q(:,:),t(:,:))
 CALL compute_one ( nz , q ( : , : ) , t ( : , : ) , nproma = nproma )
!$omp target update from(q(:,:),t(:,:))
!$omp end target data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction8

