MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , z , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , INTENT(INOUT) :: z ( : , : )
  REAL :: tmp
  REAL :: tmp2
  INTEGER :: k
  INTEGER :: proma

!$acc data present(t,q,z)
!$acc parallel
!$acc loop gang vector
  DO proma = 1 , nproma , 1
!$acc loop seq
   DO k = 1 , nz , 1
    IF ( t ( proma , k ) > 0. ) THEN
     IF ( k < 10 ) THEN
      tmp = tmp + q ( proma , k )
      q ( proma , k ) = q ( proma , k ) / t ( proma , k )
     END IF
    ELSE
     q ( proma , k ) = q ( proma , k ) * z ( proma , k )
    END IF
    tmp2 = tmp + q ( proma , k )
    z ( proma , k ) = z ( proma , k ) * tmp
   END DO
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE compute_column

END MODULE mo_column

