PROGRAM test_abstraction32
 USE mo_column , ONLY: compute
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 20 , 1 : 60 )
 REAL :: s ( 1 : 20 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p

 nproma = 20
 nz = 60
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
 END DO
!$omp target data map(alloc:q(:,:),t(:,:),s(:))
!$omp target update to(q(:,:),t(:,:),s(:))
 CALL compute ( nz , q ( : , : ) , t ( : , : ) , s ( : ) , nproma = nproma )
!$omp target update from(q(:,:),t(:,:),s(:))
!$omp end target data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction32

