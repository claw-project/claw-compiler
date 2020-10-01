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

  c = 5.345
  DO k = 2 , nz , 1
   DO p = pstart , pend , 1
    t ( p , k ) = c * k
    q ( p , k ) = q ( p , k - 1 ) + t ( p , k ) * c
   END DO
  END DO
  DO p = pstart , pend , 1
   q ( p , nz ) = q ( p , nz ) * c
  END DO
 END SUBROUTINE compute_column

END MODULE mo_column

