PROGRAM vector_loop

 CALL claw ( )
END PROGRAM vector_loop

SUBROUTINE claw ( )
 INTEGER :: j
 INTEGER :: i = 10
 INTEGER :: vec1 ( 0 : 9 )

 DO j = 0 , i , 1
  vec1 ( j ) = j
 END DO
!$claw loop-vector
 vec1 ( 0 : i ) = vec1 ( 0 : i ) + 10
 PRINT * , vec1
END SUBROUTINE claw

