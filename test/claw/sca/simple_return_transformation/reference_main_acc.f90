PROGRAM test_abstraction1
 USE mo_column , ONLY: compute_column
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
!$acc data pcreate(q(:,:),t(:,:))
!$acc update device(q(:,:),t(:,:))
 CALL compute_column ( nz , q ( : , : ) , t ( : , : ) , .TRUE. , nproma =&
  nproma )
!$acc update host(q(:,:),t(:,:))
!$acc end data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction1

