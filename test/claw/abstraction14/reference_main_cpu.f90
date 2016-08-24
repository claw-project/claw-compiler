PROGRAM test_abstraction14
 USE mo_column , ONLY: compute
 REAL :: q ( 1 : 20 , 1 : 60 , 1 : 2 )
 REAL :: t ( 1 : 60 , 1 : 20 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p
 INTEGER :: b

 nproma = 20
 nz = 60
 b = 20
 DO p = 1 , nproma , 1
  q ( p , 1 , 1 ) = 0.0
  q ( p , 1 , 2 ) = 0.0
 END DO
 DO p = 1 , nproma , 1
  t ( 1 , p ) = 0.0
 END DO
!$ACC data copyin(q,t) copyout(q,t)
 CALL compute ( nz , b , q , t , nproma = nproma )
!$ACC end data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction14

