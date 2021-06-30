PROGRAM test_abstraction18
 USE mo_column , ONLY: compute_all
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
 CALL compute_all ( nz , q ( : , : ) , t ( : , : ) , nproma = nproma )
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction18

