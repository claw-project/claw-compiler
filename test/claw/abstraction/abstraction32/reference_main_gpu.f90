PROGRAM test_abstraction4
 USE mo_column , ONLY: compute
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 20 , 1 : 60 )
 REAL :: z ( 1 : 20 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p

 nproma = 20
 nz = 60
 z = 10.0
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
 END DO
!$acc data  pcreate(q(:,1:60),t(:,1:60),z(:))
!$acc update device(q(:,1:60),t(:,1:60),z(:))
 CALL compute ( nz , q ( : , 1 : 60 ) , t ( : , 1 : 60 ) , z ( : ) , nproma =&
  nproma )
!$acc update host(q(:,1:60),t(:,1:60),z(:))
!$acc end data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction4

