PROGRAM loop_fusion

 CALL clawloop ( )
END PROGRAM loop_fusion

SUBROUTINE clawloop ( )

 INTEGER :: i
 INTEGER :: j
 INTEGER :: k
 INTEGER :: iend = 2
 INTEGER :: jend = 4
 INTEGER :: kend = 2

!$ACC parallel
 DO i = 0 , iend , 1
  DO j = 0 , jend , 1
   DO k = 0 , kend , 1
    IF ( i == 0 ) THEN
     PRINT * ,"First iteration of i" , i ,"/" , j ,"/" , k
    END IF
!$OMP parallel do
    PRINT * ,"First loop body:" , i ,"/" , j ,"/" , k
!$OMP end parallel do
!$OMP parallel do
    IF ( j .ge. 2 ) THEN
     PRINT * ,"Second loop body:" , i ,"/" , j ,"/" , k
    END IF
!$OMP end parallel do
   END DO
  END DO
 END DO
!$ACC end parallel
END SUBROUTINE clawloop

