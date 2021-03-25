PROGRAM test_abstraction1
 USE mo_column , ONLY: compute_column
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 20 , 1 : 60 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p
 INTEGER :: pstart
 INTEGER :: pend

 nproma = 20
 nz = 60
 pstart = 1
 pend = nproma
 DO p = pstart , pend , 1
  q ( p , 1 ) = 0.0
  t ( p , 1 ) = 0.0
 END DO
!$acc data pcreate(q(:,:),t(:,:))
!$acc update device(q(:,:),t(:,:))
 CALL compute_column ( nz , q ( : , : ) , t ( : , : ) , nproma = nproma ,&
  pstart = pstart , pend = pend )
!$acc update host(q(:,:),t(:,:))
!$acc end data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction1

