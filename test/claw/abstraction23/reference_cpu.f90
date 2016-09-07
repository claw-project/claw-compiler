MODULE mo_column

CONTAINS
 FUNCTION compute_column ( nz , q , t , nproma ) RESULT(res)
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  REAL :: res ( 1 : nz , 1 : nproma )
  INTEGER , INTENT(IN) :: nproma
  INTEGER :: proma

  c = 5.345
  DO k = 2 , nz , 1
   DO proma = 1 , nproma , 1
    t ( k , proma ) = c * k
   END DO
   DO proma = 1 , nproma , 1
    q ( proma , k ) = t ( k - 1 , proma ) + t ( k , proma ) * c
   END DO
  END DO
  DO proma = 1 , nproma , 1
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
  res = t
 END FUNCTION compute_column

 SUBROUTINE compute_all ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , TARGET :: z ( 1 : nproma , 1 : nz )
  INTEGER :: k
  REAL , POINTER :: zp ( : , : )
  REAL , TARGET :: res ( 1 : nz , 1 : nproma )
  REAL , POINTER :: res_p ( : , : )
  INTEGER , INTENT(IN) :: nproma
  INTEGER :: iter_nproma

  DO k = 1 , nz , 1
   DO iter_nproma = 1 , nproma , 1
    z ( iter_nproma , k ) = t ( k , iter_nproma ) + q ( iter_nproma , k )
   END DO
  END DO
  zp => z
  res ( 1 : , : ) = compute_column ( nz , q , t , nproma = nproma )
  res_p => res
 END SUBROUTINE compute_all

END MODULE mo_column

