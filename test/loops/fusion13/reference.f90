PROGRAM loop_fusion

 CALL clawloop ( )
END PROGRAM loop_fusion

SUBROUTINE clawloop ( )

 INTEGER :: i
 INTEGER :: k

!$OMP parallel
 DO k = 1 , 10 , 1
!$OMP do
  DO i = 1 , 10 , 1
   PRINT * ,"First loop body:" , i
   PRINT * ,"Second loop body:" , i
  END DO
!$OMP end do
 END DO
!$OMP end parallel
END SUBROUTINE clawloop

