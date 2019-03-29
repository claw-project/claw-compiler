MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( p1 , nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  INTEGER , INTENT(IN) :: p1
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER , PARAMETER :: c0 = 8.0
  INTEGER :: proma

!$omp target
!$omp teams
!$omp distribute
  DO proma = 1 , nproma , 1
   IF ( p1 == c0 ) THEN
    DO k = 2 , nz , 1
     t ( proma , k ) = c * k
     q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c
    END DO
   END IF
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$omp end distribute
!$omp end teams
!$omp end target
 END SUBROUTINE compute_column

END MODULE mo_column

