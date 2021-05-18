MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , l , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  LOGICAL , INTENT(IN) :: l
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER :: proma

!$omp target
!$omp teams
!$omp distribute
  DO proma = 1 , nproma , 1
   IF ( ( .NOT. l ) ) THEN
!cdir CLAW: RETURN statement transformed for parallel region
   ELSE
    c = 5.345
    DO k = 2 , nz , 1
     t ( proma , k ) = c * k
     q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c
    END DO
    q ( proma , nz ) = q ( proma , nz ) * c
   END IF
  END DO
!$omp end distribute
!$omp end teams
!$omp end target
 END SUBROUTINE compute_column

END MODULE mo_column

