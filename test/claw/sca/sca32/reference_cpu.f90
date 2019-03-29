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
  REAL , ALLOCATABLE :: y ( : )
  INTEGER :: k
  REAL :: c
  INTEGER :: proma

  IF ( ( .NOT. allocated ( y ) ) ) THEN
   ALLOCATE ( y ( nz ) )
  END IF
  c = 5.345
  DO k = 2 , nz , 1
   DO proma = 1 , nproma , 1
    t ( proma , k ) = c * k
    y ( k ) = t ( proma , k ) + s ( proma )
    q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c + y ( k )
   END DO
  END DO
  DO proma = 1 , nproma , 1
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
  IF ( allocated ( y ) ) THEN
   DEALLOCATE ( y )
  END IF
 END SUBROUTINE compute_column

END MODULE mo_column

