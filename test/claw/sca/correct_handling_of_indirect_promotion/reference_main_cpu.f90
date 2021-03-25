PROGRAM test_abstraction36
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
  t ( p , 1 ) = 0.0
 END DO
 CALL compute ( nz , q ( : , : ) , t ( : , : ) , z ( : ) , nproma = nproma )
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction36

