PROGRAM loop_fusion

 CALL clawloop ( )
END PROGRAM loop_fusion

SUBROUTINE clawloop ( )
 INTEGER :: i

!$claw loop-fusion target(cpu)
 DO i = 1 , 10 , 1
  PRINT * ,"First loop body:" , i
 END DO
!$claw loop-fusion target(cpu)
 DO i = 1 , 10 , 1
  PRINT * ,"Second loop body:" , i
 END DO
 DO i = 1 , 10 , 1
  PRINT * ,"Third loop body:" , i
  PRINT * ,"Fourth loop body:" , i
 END DO
END SUBROUTINE clawloop

