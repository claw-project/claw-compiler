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

!$acc data present(t,q,z)
!$acc parallel
!$acc loop gang vector private(tmp3)
  DO proma = 1 , nproma , 1
!$acc loop seq
   DO k = 1 , nz , 1
    q ( proma , k ) = q ( proma , k ) / t ( proma , k )
   END DO
   IF ( flag ) THEN
!$acc loop seq
    DO k = 1 , nz , 1
     IF ( z ( proma , k ) < tmp ) THEN
      z ( proma , k ) = z ( proma , k ) * tmp2
     END IF
    END DO
   END IF
!$acc loop seq
   DO k = 1 , nz , 1
    IF ( flag ) THEN
     z ( proma , k ) = z ( proma , k ) + tmp
!$acc loop seq
     DO j = 1 , 5 , 1
      IF ( flag2 ) THEN
       z ( proma , k ) = z ( proma , k ) * tmp3 ( k , j )
      END IF
     END DO
    END IF
   END DO
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE compute_column

END MODULE mod1

