MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , nx , ny )
  INTEGER , INTENT(IN) :: ny
  INTEGER , INTENT(IN) :: nx

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : , : )
  REAL , INTENT(INOUT) :: q ( : , : , : )
  INTEGER :: k
  REAL :: c
  REAL :: d
  INTEGER :: i
  INTEGER :: j

  c = 5.345
  DO k = 2 , nz , 1
   DO j = 1 , ny , 1
    DO i = 1 , nx , 1
     t ( i , j , k ) = c * k
     d = t ( i , j , k ) + c
     q ( i , j , k ) = q ( i , j , k - 1 ) + t ( i , j , k ) * c + d
    END DO
   END DO
  END DO
  DO j = 1 , ny , 1
   DO i = 1 , nx , 1
    q ( i , j , nz ) = q ( i , j , nz ) * c
   END DO
  END DO
 END SUBROUTINE compute_column

END MODULE mo_column

