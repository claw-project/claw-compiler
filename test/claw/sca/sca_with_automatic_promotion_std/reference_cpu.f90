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
    z ( proma , k ) = t ( proma , k ) + q ( proma , k )
   END DO
  END DO
  DO k = 1 , nz , 1
   DO proma = 1 , nproma , 1
    z ( proma , k ) = t ( proma , k ) + q ( proma , k )
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

  c = 5.345
  DO k = 2 , nz , 1
   DO proma = 1 , nproma , 1
    t ( proma , k ) = c * k
    q ( proma , k ) = t ( proma , k - 1 ) + t ( proma , k ) * c
   END DO
  END DO
  DO proma = 1 , nproma , 1
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
 END SUBROUTINE compute_column

END MODULE mo_column

