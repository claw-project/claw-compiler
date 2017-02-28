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

  c = 5.345
  DO k = 2 , nz , 1
   DO proma = 1 , nproma , 1
    t ( proma , k ) = c * k
   END DO
   DO proma = 1 , nproma , 1
    q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c
   END DO
  END DO
  DO proma = 1 , nproma , 1
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
 END SUBROUTINE compute_column

END MODULE mo_column

