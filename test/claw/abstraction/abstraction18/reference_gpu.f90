MODULE mo_column

CONTAINS
 SUBROUTINE compute_all ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , TARGET :: z ( 1 : nproma , 1 : nz )
  INTEGER :: k
  REAL , POINTER :: zp ( : , : )

  INTEGER , INTENT(IN) :: nproma
  INTEGER :: iter_nproma


  DO k = 1 , nz , 1
   DO iter_nproma = 1 , nproma , 1
    z ( iter_nproma , k ) = t ( iter_nproma , k ) + q ( iter_nproma , k )
   END DO
  END DO
  DO k = 1 , nz , 1
   DO iter_nproma = 1 , nproma , 1
    z ( iter_nproma , k ) = t ( iter_nproma , k ) + q ( iter_nproma , k )
   END DO
  END DO
  zp => z
  CALL compute_column ( nz , q , t , nproma = nproma )
 END SUBROUTINE compute_all

 SUBROUTINE compute_column ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c

  INTEGER , INTENT(IN) :: nproma
  INTEGER :: proma


!$acc data present(t,q)
!$acc parallel
!$acc loop gang vector
  DO proma = 1 , nproma , 1
   c = 5.345
!$acc loop seq
   DO k = 2 , nz , 1
    t ( proma , k ) = c * k
    q ( proma , k ) = t ( proma , k - 1 ) + t ( proma , k ) * c
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE compute_column

END MODULE mo_column

