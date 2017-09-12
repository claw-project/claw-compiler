PROGRAM test_abstraction24
 USE mo_column , ONLY: compute_all
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 60 , 1 : 20 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p

 nproma = 20
 nz = 60
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
  t ( 1 , p ) = 0.0
 END DO
!$ACC data copyin(q,t) copyout(q,t)
 CALL compute_all ( nz , q , t , nproma = nproma )
!$ACC end data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction24

