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

!$acc data present(t,q)
!$acc parallel
!$acc loop gang vector collapse(2)
  DO j = 1 , ny , 1
   DO i = 1 , nx , 1
    c = 5.345
!$acc loop seq
    DO k = 2 , nz , 1
     t ( i , j , k ) = c * k
     d = t ( i , j , k ) + c
     q ( i , j , k ) = q ( i , j , k - 1 ) + t ( i , j , k ) * c + d
    END DO
    q ( i , j , nz ) = q ( i , j , nz ) * c
   END DO
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE compute_column

END MODULE mo_column

