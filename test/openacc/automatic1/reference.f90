PROGRAM automatic_openacc
 REAL :: g1 ( 1 : 10 )
 REAL :: ga ( 1 : 10 )
 REAL :: gb ( 1 : 10 )

 ga ( : ) = 2
 gb ( : ) = 0
 g1 ( : ) = 1
 CALL my_subroutine ( ga , gb )

CONTAINS
 SUBROUTINE my_subroutine ( a , b )

  REAL , INTENT(IN) :: a ( 1 : 10 )
  REAL , INTENT(OUT) :: b ( 1 : 10 )
  REAL :: loc1 ( 1 : 10 )
  INTEGER :: i

!$acc data present(a,b,g1) pcreate(loc1)
  DO i = 1 , 10 , 1
   loc1 ( i ) = a ( i ) + 10 + g1 ( i )
   b ( i ) = loc1 ( i ) / 2
  END DO
!$acc end data
 END SUBROUTINE my_subroutine

END PROGRAM automatic_openacc

