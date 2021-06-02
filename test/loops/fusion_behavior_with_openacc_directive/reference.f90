PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop ( )

 INTEGER :: i
 INTEGER :: k

!$ACC parallel
!$ACC loop seq
 DO k = 1 , 10 , 1
!$ACC loop
  DO i = 1 , 10 , 1
   PRINT * ,"First loop body:" , i
   PRINT * ,"Second loop body:" , i
  END DO
 END DO
!$ACC end parallel
END SUBROUTINE clawloop

