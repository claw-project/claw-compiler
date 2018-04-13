MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , z , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , INTENT(INOUT) :: z ( : , : )
  REAL :: tmp ( 1 : nproma )
  REAL :: tmp2 ( 1 : nproma )
  INTEGER :: k
  INTEGER :: proma

  DO k = 1 , nz , 1
   DO proma = 1 , nproma , 1
    IF ( t ( proma , k ) > 0. ) THEN
     IF ( k < 10 ) THEN
      tmp ( proma ) = tmp ( proma ) + q ( proma , k )
      q ( proma , k ) = q ( proma , k ) / t ( proma , k )
     END IF
    ELSE
     q ( proma , k ) = q ( proma , k ) * z ( proma , k )
    END IF
   END DO
   DO proma = 1 , nproma , 1
    tmp2 ( proma ) = tmp ( proma ) + q ( proma , k )
   END DO
   DO proma = 1 , nproma , 1
    z ( proma , k ) = z ( proma , k ) * tmp ( proma )
   END DO
  END DO
 END SUBROUTINE compute_column

END MODULE mo_column

