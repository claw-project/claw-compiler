MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER :: proma

!$omp target
!$omp teams
!$omp distribute  collapse(2)
  DO proma = 1 , nproma , 1
   DO k = 2 , nz , 1
    c = 5.345
    t ( proma , k ) = c * k
    q ( proma , k ) = q ( proma , k ) + t ( proma , k ) * c
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$omp end distribute
!$omp end teams
!$omp end target
 END SUBROUTINE compute_column

END MODULE mo_column

