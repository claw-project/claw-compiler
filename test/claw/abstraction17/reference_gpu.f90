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

  c = 5.345
!$acc parallel
!$acc loop
  DO proma = 1 , nproma , 1
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$acc end parallel

 CONTAINS
  FUNCTION test_contains ( )
   INTEGER :: test_contains

   test_contains = 10
  END FUNCTION test_contains

 END SUBROUTINE compute_column

END MODULE mo_column

