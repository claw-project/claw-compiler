PROGRAM LOOP_INTERCHANGE
  CALL clawloop
END

SUBROUTINE clawloop ( )

 INTEGER :: i
 INTEGER :: j
 INTEGER :: k
 INTEGER :: a
 INTEGER :: b
 INTEGER :: c
 INTEGER :: x
 INTEGER :: y
 INTEGER :: z

 DO k = 1 , 2 , 1
  DO i = 1 , 4 , 1
   DO j = 1 , 3 , 1
    PRINT * ,"Iteration i=" , i ,", j=" , j ,", k=" , k
   END DO
  END DO
 END DO
 DO b = 1 , 3 , 1
  DO c = 1 , 2 , 1
   DO a = 1 , 4 , 1
    PRINT * ,"Iteration a=" , a ,", b=" , b ,", c=" , c
   END DO
  END DO
 END DO
 DO x = 1 , 4 , 1
  DO z = 1 , 2 , 1
   DO y = 1 , 3 , 1
    PRINT * ,"Iteration x=" , x ,", y=" , y ,", z=" , z
   END DO
  END DO
 END DO
END SUBROUTINE clawloop

