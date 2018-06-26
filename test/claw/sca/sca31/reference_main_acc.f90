PROGRAM test_abstraction31
 USE mo_column , ONLY: compute , t1
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: z ( 1 : 20 )
 TYPE ( t1 ) :: ty
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p

 nproma = 20
 nz = 60
 z = 10.0
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
 END DO
 ALLOCATE ( ty % y ( nproma , nz ) )
!$acc data pcreate(q(:,:),ty%y(:,:),z(:))
!$acc update device(q(:,:),ty%y(:,:),z(:))
 CALL compute ( nz , q ( : , : ) , ty % y ( : , : ) , z ( : ) , nproma =&
  nproma )
!$acc update host(q(:,:),ty%y(:,:),z(:))
!$acc end data
 PRINT * , sum ( q )
 PRINT * , sum ( ty % y )
 DEALLOCATE ( ty % y )
END PROGRAM test_abstraction31

