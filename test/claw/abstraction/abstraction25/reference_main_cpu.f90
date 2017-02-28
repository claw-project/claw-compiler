PROGRAM test_abstraction25
 USE mo_column , ONLY: compute
 REAL :: q ( 1 : 20 , 1 : 60 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p

 nproma = 20
 nz = 60
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
 END DO
!$ACC data copyin(q,t) copyout(q,t)
 CALL compute ( nz , q , nproma = nproma )
!$ACC end data
 PRINT * , sum ( q )
END PROGRAM test_abstraction25

