PROGRAM VECTOR_LOOP
  CALL claw
END

SUBROUTINE claw ( )
 INTEGER :: i = 10
 INTEGER :: vec1 ( 0 : 10 , 0 : 10 )
 INTEGER :: vec2 ( 0 : 10 , 0 : 10 )

 INTEGER :: j1
 INTEGER :: j3

 vec1 ( 0 : i , 0 : i ) = 0
 vec2 ( 0 : i , 0 : i ) = 100
 DO j1 = 0 , i
  DO j3 = 0 , i
   vec1 ( j1 , j3 ) = vec2 ( j1 , j3 ) + 10
  END DO
 END DO
 PRINT * , vec1
 PRINT * , vec2
END SUBROUTINE claw

