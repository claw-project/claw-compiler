MODULE mo_column

CONTAINS
 SUBROUTINE compute ( nz , q , t , z , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , INTENT(INOUT) :: z ( 1 : nproma )

  CALL compute_column ( nz , q , t , z , nproma = nproma )
 END SUBROUTINE compute

 SUBROUTINE compute_column ( nz , q , t , z , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , INTENT(INOUT) :: z ( 1 : nproma )
  INTEGER :: k
  REAL :: c
  REAL :: p
  INTEGER :: proma

!$acc data present(t,q,z)
!$acc parallel
!$acc loop gang vector
  DO proma = 1 , nproma , 1
   c = 5.345
   p = c ** ( 2.0 )
!$acc loop seq
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    p = t ( proma , k ) + 1.0
    q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c
    IF ( p > 2.0 ) THEN
     q ( proma , k ) = q ( proma , k - 1 ) + t ( proma , k ) * c * 2.0
    END IF
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE compute_column

END MODULE mo_column

