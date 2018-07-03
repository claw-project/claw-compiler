PROGRAM loop_fusion

 CALL clawloop ( )
END PROGRAM loop_fusion

SUBROUTINE clawloop ( )

 INTEGER :: i

 DO i = 1 , 10 , 1
  PRINT * ,"First loop body:" , i
 END DO
 DO i = 1 , 10 , 1
  PRINT * ,"Second loop body:" , i
 END DO
 DO i = 1 , 10 , 1
  PRINT * ,"Third loop body:" , i
 END DO
 DO i = 1 , 10 , 1
  PRINT * ,"Fourth loop body:" , i
 END DO
END SUBROUTINE clawloop

