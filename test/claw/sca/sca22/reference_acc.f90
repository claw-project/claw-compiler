MODULE mo_column

CONTAINS
 SUBROUTINE compute_all ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , TARGET :: z ( 1 : nproma , 1 : nz )
  INTEGER :: k
  REAL , POINTER :: zp ( : , : )
  INTEGER :: proma

  DO k = 1 , nz , 1
   DO proma = 1 , nproma , 1
    z ( proma , k ) = t ( k , proma ) + q ( proma , k )
   END DO
  END DO
  zp => z
  CALL compute_column ( nz , q , t , nproma = nproma )
 END SUBROUTINE compute_all

 SUBROUTINE compute_column ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER :: proma

!$acc data present(t,q)
!$acc parallel
!$acc loop gang vector
  DO proma = 1 , nproma , 1
   c = 5.345
!$acc loop seq
   DO k = 2 , nz , 1
    t ( k , proma ) = c * k
    q ( proma , k ) = t ( k - 1 , proma ) + t ( k , proma ) * c
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE compute_column

END MODULE mo_column

