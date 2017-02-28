MODULE mo_column

CONTAINS
 SUBROUTINE compute_column ( nz , q , t , nx , ny )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : , : )
  REAL , INTENT(INOUT) :: q ( : , : , : )
  INTEGER :: k
  REAL :: c
  REAL :: d ( 1 : nx , 1 : ny )
  INTEGER , INTENT(IN) :: nx
  INTEGER :: i
  INTEGER , INTENT(IN) :: ny
  INTEGER :: j

  c = 5.345
  DO k = 2 , nz , 1
   DO j = 1 , ny , 1
    DO i = 1 , nx , 1
     t ( i , j , k ) = c * k
    END DO
   END DO
   DO j = 1 , ny , 1
    DO i = 1 , nx , 1
     d ( i , j ) = t ( i , j , k ) + c
    END DO
   END DO
   DO j = 1 , ny , 1
    DO i = 1 , nx , 1
     q ( i , j , k ) = q ( i , j , k - 1 ) + t ( i , j , k ) * c + d ( i , j )
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

