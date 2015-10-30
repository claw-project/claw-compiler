PROGRAM loop_interchange

 CALL clawloop ( )
END PROGRAM loop_interchange

SUBROUTINE clawloop ( )
 INTEGER :: i

!$claw loop-fusion
 DO i = 1 , 10 , 1
  PRINT * ,"First loop body:" , i
 END DO
!$claw loop-fusion
 DO i = 1 , 10 , 1
  PRINT * ,"Second loop body:" , i
 END DO
END SUBROUTINE clawloop

