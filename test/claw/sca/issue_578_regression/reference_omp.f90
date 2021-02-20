MODULE test_index_mod

CONTAINS
 SUBROUTINE top_p ( vert , p , im , jm )
  INTEGER , INTENT(IN) :: jm
  INTEGER , INTENT(IN) :: im

  INTEGER , INTENT(IN) :: vert
  REAL , INTENT(OUT) :: p ( 1 : im , 1 : jm , 1 : vert )
  INTEGER :: i
  INTEGER :: j

!$omp target
!$omp teams
!$omp distribute  collapse(2)
  DO j = 1 , jm , 1
   DO i = 1 , im , 1
    CALL set_p ( vert , p ( i , j , : ) )
   END DO
  END DO
!$omp end distribute
!$omp end teams
!$omp end target
 END SUBROUTINE top_p

 SUBROUTINE set_p ( vert , p )

  INTEGER , INTENT(IN) :: vert
  REAL , INTENT(OUT) :: p ( 1 : vert )
  INTEGER :: k

!$omp declare target
!$omp single
  DO k = 1 , vert , 1
   p ( k ) = 1.0
  END DO
 END SUBROUTINE set_p

END MODULE test_index_mod

