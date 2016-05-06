PROGRAM compile_guard_test

 CALL dummy_subroutine ( )
END PROGRAM compile_guard_test

SUBROUTINE dummy_subroutine ( )
 INTEGER :: i

!$claw loop-fusion
 DO i = 1 , 10 , 1
  PRINT * ,"First loop body:" , i
  PRINT * ,"Second loop body:" , i
 END DO
END SUBROUTINE dummy_subroutine

