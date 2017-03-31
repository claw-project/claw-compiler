PROGRAM loop_fusion

 CALL clawloop ( )
END PROGRAM loop_fusion

SUBROUTINE clawloop ( )
 INTEGER :: i

 DO i = 1 , 10 , 1
  PRINT * ,"First loop body:" , i
  PRINT * ,"Second loop body:" , i
  PRINT * ,"Third loop body:" , i
 END DO
END SUBROUTINE clawloop

