MODULE mo_column
 TYPE :: ty_column
  CONTAINS
  PROCEDURE :: compute_column
 END TYPE ty_column

CONTAINS
 SUBROUTINE compute_column ( this , nz , q , t , nproma )
  CLASS(ty_column) :: this
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER , INTENT(IN) :: nproma
  INTEGER :: proma

!$acc data present(t,q,nproma,nz)
!$acc parallel private(this,k,proma,c)
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
 END SUBROUTINE compute_column

END MODULE mo_column

