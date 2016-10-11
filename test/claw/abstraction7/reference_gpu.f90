MODULE mo_column

CONTAINS
 FUNCTION compute_column ( nz , q , t , nproma ) RESULT(r)
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER :: r
  INTEGER , INTENT(IN) :: nproma
  INTEGER :: proma

!$acc data present(t,q,nproma,nz)
!$acc parallel private(k,proma,r,c)
!$acc loop
  DO proma = 1 , nproma , 1
   c = 5.345
!$acc loop seq
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$acc end parallel
!$acc end data
 END FUNCTION compute_column

 SUBROUTINE compute ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: result
  INTEGER , INTENT(IN) :: nproma

  result = compute_column ( nz , q , t , nproma = nproma )
 END SUBROUTINE compute

END MODULE mo_column

