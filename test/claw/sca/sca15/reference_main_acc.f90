PROGRAM test_abstraction15
 USE mo_column , ONLY: compute
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 60 , 1 : 20 )
 REAL :: z ( 1 : 60 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p
 INTEGER :: b

 nproma = 20
 nz = 60
 b = 20
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
  z ( p ) = 0.0
  t ( 1 , p ) = 0.0
 END DO
!$acc data pcreate(q(:,:),t(:,:),z(:))
!$acc update device(q(:,:),t(:,:),z(:))
 CALL compute ( nz , b , q ( : , : ) , t ( : , : ) , z ( : ) , nproma = nproma&
  )
!$acc update host(q(:,:),t(:,:),z(:))
!$acc end data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction15

