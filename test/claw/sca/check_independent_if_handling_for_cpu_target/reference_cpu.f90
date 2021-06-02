MODULE mod1

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , z , flag , flag2 , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( 1 : nproma , 1 : nz )
  REAL , INTENT(INOUT) :: q ( 1 : nproma , 1 : nz )
  REAL , INTENT(INOUT) :: z ( 1 : nproma , 1 : nz )
  LOGICAL , INTENT(IN) :: flag
  LOGICAL , INTENT(IN) :: flag2
  REAL :: tmp
  REAL :: tmp2
  REAL :: tmp3 ( 1 : nz , 1 : 5 )
  INTEGER :: k
  INTEGER :: j
  INTEGER :: proma

  DO k = 1 , nz , 1
   DO proma = 1 , nproma , 1
    q ( proma , k ) = q ( proma , k ) / t ( proma , k )
   END DO
  END DO
  IF ( flag ) THEN
   DO k = 1 , nz , 1
    DO proma = 1 , nproma , 1
     IF ( z ( proma , k ) < tmp ) THEN
      z ( proma , k ) = z ( proma , k ) * tmp2
     END IF
    END DO
   END DO
  END IF
  DO k = 1 , nz , 1
   IF ( flag ) THEN
    DO proma = 1 , nproma , 1
     z ( proma , k ) = z ( proma , k ) + tmp
    END DO
    DO j = 1 , 5 , 1
     IF ( flag2 ) THEN
      DO proma = 1 , nproma , 1
       z ( proma , k ) = z ( proma , k ) * tmp3 ( k , j )
      END DO
     END IF
    END DO
   END IF
  END DO
 END SUBROUTINE compute_column

END MODULE mod1

