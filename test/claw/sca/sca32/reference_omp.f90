MODULE mo_column

CONTAINS
 SUBROUTINE compute ( nz , q , t , s , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , INTENT(INOUT) :: s ( 1 : nproma )

  CALL compute_column ( nz , q , t , s , nproma = nproma )
 END SUBROUTINE compute

 SUBROUTINE compute_column ( nz , q , t , s , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , INTENT(INOUT) :: s ( 1 : nproma )
  REAL , ALLOCATABLE :: y ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER :: proma

  IF ( ( .NOT. allocated ( y ) ) ) THEN
   ALLOCATE ( y ( nproma , nz ) )
  END IF
!$omp target data map(alloc:y)
!$omp target
!$omp teams
!$omp distribute
  DO proma = 1 , nproma , 1
   c = 5.345
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    y ( proma , k ) = t ( proma , k ) + s ( proma )
    q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c + y ( proma ,&
     k )
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$omp end distribute
!$omp end teams
!$omp end target
!$omp end target data
  IF ( allocated ( y ) ) THEN
   DEALLOCATE ( y )
  END IF
 END SUBROUTINE compute_column

END MODULE mo_column

