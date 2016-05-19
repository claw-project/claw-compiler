MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER , INTENT(IN) :: nproma
  INTEGER :: proma

  DO proma = 1 , nproma , 1
   c = 5.345
   DO k = 1 , nz , 1
    t ( nproma , k ) = c * k
    q ( nproma , k ) = q ( nproma , k - 1 ) + t ( nproma , k ) * c
   END DO
   q ( nproma , nz ) = q ( nproma , nz ) * c
  END DO
 END SUBROUTINE compute_column

END MODULE mo_column

