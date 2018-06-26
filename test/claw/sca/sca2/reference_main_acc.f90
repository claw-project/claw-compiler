PROGRAM test_abstraction2
 USE mo_column , ONLY: compute_column
 REAL :: q ( 1 : 10 , 1 : 10 , 1 : 60 )
 REAL :: t ( 1 : 10 , 1 : 10 , 1 : 60 )
 INTEGER :: nx
 INTEGER :: ny
 INTEGER :: nz
 INTEGER :: i
 INTEGER :: j

 nx = 10
 ny = 10
 nz = 60
 DO i = 1 , nx , 1
  DO j = 1 , ny , 1
   q ( i , j , 1 ) = 0.0
   t ( i , j , 1 ) = 0.0
  END DO
 END DO
!$acc data pcreate(q(:,:,:),t(:,:,:))
!$acc update device(q(:,:,:),t(:,:,:))
 CALL compute_column ( nz , q ( : , : , : ) , t ( : , : , : ) , nx = nx , ny =&
  ny )
!$acc update host(q(:,:,:),t(:,:,:))
!$acc end data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction2

