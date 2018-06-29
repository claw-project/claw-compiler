PROGRAM test_abstraction19
 USE mo_column , ONLY: compute
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 60 , 1 : 20 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p
 INTEGER :: b

 nproma = 20
 nz = 60
 b = 20
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
  t ( 1 , p ) = 0.0
 END DO
!$acc data pcreate(q(:,:),t(:,:))
!$acc update device(q(:,:),t(:,:))
 CALL compute ( nz , b , q ( : , : ) , t ( : , : ) , nproma = nproma )
!$acc update host(q(:,:),t(:,:))
!$acc end data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction19

