MODULE mo_column

CONTAINS
 SUBROUTINE compute ( nz , q , t , z , nproma )

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , INTENT(INOUT) :: z ( 1 : nproma )
  INTEGER , INTENT(IN) :: nproma

  CALL compute_column ( nz , q , t , z , nproma = nproma )
 END SUBROUTINE compute

 SUBROUTINE compute_column ( nz , q , t , z , nproma )

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , INTENT(INOUT) :: z ( 1 : nproma )
  INTEGER , INTENT(IN) :: nproma
  REAL , ALLOCATABLE :: y ( : )
  INTEGER :: k
  REAL :: c
  INTEGER :: proma

  ALLOCATE ( y ( nz ) )
!$acc data present(t,q,z)
!$acc parallel
!$acc loop gang vector
  DO proma = 1 , nproma , 1
   c = 5.345
!$acc loop seq
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$acc end parallel
!$acc end data
  DEALLOCATE ( y )
 END SUBROUTINE compute_column

END MODULE mo_column

