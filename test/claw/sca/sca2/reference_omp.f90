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

!$omp target
!$omp teams thread_limit(256) num_teams(65536)
!$omp distribute dist_schedule(static, 256) collapse(2)
  DO j = 1 , ny , 1
   DO i = 1 , nx , 1
    c = 5.345
    DO k = 2 , nz , 1
     t ( i , j , k ) = c * k
     d = t ( i , j , k ) + c
     q ( i , j , k ) = q ( i , j , k - 1 ) + t ( i , j , k ) * c + d
    END DO
    q ( i , j , nz ) = q ( i , j , nz ) * c
   END DO
  END DO
!$omp end distribute
!$omp end teams
!$omp end target
 END SUBROUTINE compute_column

END MODULE mo_column

