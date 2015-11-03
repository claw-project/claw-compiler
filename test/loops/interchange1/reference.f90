PROGRAM loop_interchange

 CALL clawloop ( )
END PROGRAM loop_interchange

SUBROUTINE clawloop ( )
 INTEGER :: i
 INTEGER :: j

!$claw loop-interchange
 DO j = 1 , 2 , 1
  DO i = 1 , 10 , 1
   PRINT * ,"Iteration " , i ,"," , j
  END DO
 END DO
END SUBROUTINE clawloop

