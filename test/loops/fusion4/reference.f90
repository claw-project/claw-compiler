PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop ( )

 INTEGER :: i
 INTEGER :: j
 INTEGER :: k

 DO i = 1 , 5 , 1
  DO j = 1 , 4 , 1
   DO k = 1 , 2 , 1
    PRINT * ,"First loop body:" , i , j
    PRINT * ,"Second loop body:" , i , j
   END DO
  END DO
 END DO
END SUBROUTINE clawloop

