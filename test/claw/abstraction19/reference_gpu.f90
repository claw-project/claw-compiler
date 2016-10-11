MODULE mo_column

CONTAINS
 FUNCTION compute_column ( nz , b , q , t , nproma ) RESULT(r)
  INTEGER , INTENT(IN) :: nz
  INTEGER , INTENT(IN) :: b
  REAL , INTENT(INOUT) :: t ( 1 : nproma , 1 : b )
  REAL , INTENT(INOUT) :: q ( 1 : b , 1 : nproma )
  REAL :: z ( 1 : nproma )
  INTEGER :: k
  REAL :: c
  INTEGER :: r
  INTEGER , INTENT(IN) :: nproma
  INTEGER :: proma

!$acc data present(q,nproma,nz,b,t)
!$acc parallel private(r,k,proma,c,z)
!$acc loop
  DO proma = 1 , nproma , 1
   c = 5.345
!$acc loop seq
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    q ( k , proma ) = q ( k - 1 , proma ) + t ( proma , k ) * c
   END DO
   z ( proma ) = q ( nz , proma ) * 2.0
   q ( nz , proma ) = q ( nz , proma ) * c
  END DO
!$acc end parallel
!$acc end data
 END FUNCTION compute_column

 SUBROUTINE compute ( nz , b , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  INTEGER , INTENT(IN) :: b
  REAL , INTENT(INOUT) :: t ( 1 : nproma , 1 : b )
  REAL , INTENT(INOUT) :: q ( 1 : b , 1 : nproma )
  INTEGER :: result
  INTEGER , INTENT(IN) :: nproma

  result = compute_column ( nz , b , q , t , nproma = nproma )
 END SUBROUTINE compute

END MODULE mo_column

