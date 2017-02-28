PROGRAM loop_fusion

 CALL clawloop ( )
END PROGRAM loop_fusion

SUBROUTINE clawloop ( )
 INTEGER :: i

!$claw loop-fusion group(g1)
 DO i = 1 , 5 , 1
  PRINT * ,"First loop body:" , i
  PRINT * ,"Second loop body:" , i
 END DO
!$claw loop-fusion group(g2)
 DO i = 1 , 5 , 1
  PRINT * ,"Third loop body:" , i
  PRINT * ,"Fourth loop body:" , i
 END DO
!$claw loop-fusion
 DO i = 1 , 5 , 1
  PRINT * ,"Fifth loop body:" , i
  PRINT * ,"Sixth loop body:" , i
 END DO
END SUBROUTINE clawloop

