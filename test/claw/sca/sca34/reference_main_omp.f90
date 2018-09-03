PROGRAM test_column_conditional
 USE mo_column , ONLY: compute_column
 INTEGER :: i
 REAL :: q ( 1 : 5 , 1 : 10 )
 REAL :: t ( 1 : 5 , 1 : 10 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p

 nproma = 5
 nz = 10
 DO p = 1 , nproma , 1
  q ( p , 1 : nz ) = 6.0
  t ( p , 1 : 6 ) = 2.0
  t ( p , 6 : nz ) = 0.0
 END DO
!$omp target data map(alloc:q(:,:),t(:,:))
!$omp target update to(q(:,:),t(:,:))
 CALL compute_column ( nz , q ( : , : ) , t ( : , : ) , nproma = nproma )
!$omp target update from(q(:,:),t(:,:))
!$omp end target data
 PRINT * , ( q ( 1 , i ) , i = 1 , 10 , 1 )
END PROGRAM test_column_conditional

