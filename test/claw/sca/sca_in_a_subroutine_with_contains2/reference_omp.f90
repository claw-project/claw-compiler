MODULE mo_column

CONTAINS
 FUNCTION compute_column ( nz , b , q , t , nproma ) RESULT(r)
  INTEGER , INTENT(IN) :: nproma
  INTEGER , INTENT(IN) :: nz
  INTEGER , INTENT(IN) :: b
  REAL , INTENT(INOUT) :: t ( 1 : nproma , 1 : b )
  REAL , INTENT(INOUT) :: q ( 1 : b , 1 : nproma )
  REAL :: z ( 1 : nproma )
  REAL , ALLOCATABLE :: y ( : )
  INTEGER :: k
  REAL :: c
  INTEGER :: r
  INTEGER :: proma

!$omp target
!$omp teams
!$omp distribute  private(y)
  DO proma = 1 , nproma , 1
   c = 5.345
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    q ( k , proma ) = q ( k - 1 , proma ) + t ( proma , k ) * c
   END DO
   z ( proma ) = q ( nz , proma ) * 2.0
   q ( nz , proma ) = q ( nz , proma ) * c
  END DO
!$omp end distribute
!$omp end teams
!$omp end target
 END FUNCTION compute_column

 SUBROUTINE compute ( nz , b , q , t , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  INTEGER , INTENT(IN) :: b
  REAL , INTENT(INOUT) :: t ( 1 : nproma , 1 : b )
  REAL , INTENT(INOUT) :: q ( 1 : b , 1 : nproma )
  INTEGER :: result

  result = compute_column ( nz , b , q , t , nproma = nproma )
 END SUBROUTINE compute

END MODULE mo_column

