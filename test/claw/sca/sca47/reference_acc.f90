MODULE test_index_mod

CONTAINS
 SUBROUTINE top_p ( vert , p , im , jm )
  INTEGER , INTENT(IN) :: jm
  INTEGER , INTENT(IN) :: im

  INTEGER , INTENT(IN) :: vert
  REAL , INTENT(OUT) :: p ( 1 : im , 1 : jm , 1 : vert )
  INTEGER :: i
  INTEGER :: j

!$acc data present(p)
!$acc parallel
!$acc loop gang vector collapse(2)
  DO j = 1 , jm , 1
   DO i = 1 , im , 1
    CALL set_p ( vert , p ( i , j , : ) )
   END DO
  END DO
!$acc end parallel
!$acc end data
 END SUBROUTINE top_p

 SUBROUTINE set_p ( vert , p )

  INTEGER , INTENT(IN) :: vert
  REAL , INTENT(OUT) :: p ( 1 : vert )
  INTEGER :: k

!$acc routine seq
  DO k = 1 , vert , 1
   p ( k ) = 1.0
  END DO
 END SUBROUTINE set_p

END MODULE test_index_mod

