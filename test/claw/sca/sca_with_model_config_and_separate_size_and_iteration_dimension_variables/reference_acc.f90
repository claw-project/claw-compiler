MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , nproma , pstart , pend )
  INTEGER , INTENT(IN) :: pend
  INTEGER , INTENT(IN) :: pstart
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( 1 : nproma , 1 : nz )
  REAL , INTENT(INOUT) :: q ( 1 : nproma , 1 : nz )
  INTEGER :: k
  REAL :: c
  INTEGER :: p

!$acc data present(t,q)
!$acc parallel
!$acc loop gang vector
  DO p = pstart , pend , 1
   c = 5.345
!$acc loop seq
   DO k = 2 , nz , 1
    t ( p , k ) = c * k
    q ( p , k ) = q ( p , k - 1 ) + t ( p , k ) * c
   END DO
   q ( p , nz ) = q ( p , nz ) * c
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE compute_column

END MODULE mo_column

