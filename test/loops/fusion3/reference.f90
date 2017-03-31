PROGRAM loop_fusion

 CALL clawloop ( )
END PROGRAM loop_fusion

SUBROUTINE clawloop ( )
 INTEGER :: i
 INTEGER :: j

 DO i = 1 , 5 , 1
  DO j = 1 , 2 , 1
   PRINT * ,"First loop body:" , i , j
   PRINT * ,"Second loop body:" , i , j
  END DO
 END DO
END SUBROUTINE clawloop

