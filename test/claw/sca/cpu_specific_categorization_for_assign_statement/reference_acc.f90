MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  INTEGER :: proma

!$acc data present(t,q)
!$acc parallel
!$acc loop gang vector
  DO proma = 1 , nproma , 1
!$acc loop seq
   DO k = 1 , nz , 1
    IF ( t ( proma , k ) > 0. ) THEN
     q ( proma , k ) = q ( proma , k ) / t ( proma , k )
    END IF
   END DO
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE compute_column

END MODULE mo_column

