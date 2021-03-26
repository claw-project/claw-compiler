MODULE mo_column

CONTAINS
 FUNCTION compute_column ( nz , b , q , t , nproma , o ) RESULT(r)
  INTEGER , INTENT(IN) :: nproma
  INTEGER , INTENT(IN) :: nz
  INTEGER , INTENT(IN) :: b
  REAL , INTENT(INOUT) :: t ( 1 : nproma , 1 : b )
  REAL , INTENT(INOUT) :: q ( 1 : b , 1 : nproma )
  INTEGER :: k
  LOGICAL , OPTIONAL :: o
  REAL :: c
  INTEGER :: r
  INTEGER :: proma

  c = 5.345
  DO k = 2 , nz , 1
   DO proma = 1 , nproma , 1
    t ( proma , k ) = c * k
    q ( k , proma ) = q ( k - 1 , proma ) + t ( proma , k ) * c
   END DO
  END DO
  DO proma = 1 , nproma , 1
   q ( nz , proma ) = q ( nz , proma ) * c
  END DO
 END FUNCTION compute_column

 SUBROUTINE compute ( nz , b , q , t , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  INTEGER , INTENT(IN) :: b
  REAL , INTENT(INOUT) :: t ( 1 : nproma , 1 : b )
  REAL , INTENT(INOUT) :: q ( 1 : b , 1 : nproma )
  INTEGER :: result

  result = compute_column ( nz , b , q , t , nproma = nproma , o = .TRUE. )
 END SUBROUTINE compute

END MODULE mo_column

