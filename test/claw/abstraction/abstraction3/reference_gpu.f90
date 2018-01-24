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


!$omp target
!$omp parallel
!$omp do
  DO proma = 1 , nproma , 1
   c = 5.345
!$omp do
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$omp end do
!$omp end parallel
!$omp end target
 END SUBROUTINE compute_column

END MODULE mo_column

