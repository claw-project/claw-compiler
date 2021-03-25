PROGRAM LOOP_INTERCHANGE
  CALL clawloop
END

SUBROUTINE clawloop ( )

 INTEGER :: i
 INTEGER :: j

 DO j = 1 , 2 , 1
  DO i = 1 , 10 , 1
   PRINT * ,"Iteration " , i ,"," , j
  END DO
 END DO
!$claw loop-interchange target(cpu)
 DO i = 1 , 10 , 1
  DO j = 1 , 2 , 1
   PRINT * ,"Iteration " , i ,"," , j
  END DO
 END DO
END SUBROUTINE clawloop

