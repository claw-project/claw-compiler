PROGRAM test_abstraction12
 USE mo_column , ONLY: compute
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 20 , 1 : 60 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p
 INTEGER :: b

 nproma = 20
 nz = 60
 b = 20
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
  t ( p , 1 ) = 0.0
 END DO
 CALL compute ( nz , b , q ( : , : ) , t ( : , : ) , nproma = nproma )
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction12

