MODULE sca49

CONTAINS
 SUBROUTINE sub1 ( n_term )
  INTEGER , INTENT(IN) :: n_term

  INTEGER :: i
  INTEGER :: j
  INTEGER , PARAMETER :: ny = 10
  INTEGER , PARAMETER :: nx = 10
  INTEGER :: k

  DO k = 1 , n_term , 1
   DO j = 1 , 10 , 1
    DO i = 1 , 10 , 1
    END DO
   END DO
  END DO
 END SUBROUTINE sub1

END MODULE sca49

