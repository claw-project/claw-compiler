PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop ( )

 INTEGER :: i
 INTEGER :: j
 INTEGER :: k
 INTEGER :: iend = 2
 INTEGER :: jend = 4
 INTEGER :: kend = 2

 DO i = 0 , iend , 1
!$claw loop-hoist(j,k) target(cpu)
  IF ( i == 0 ) THEN
   PRINT * ,"First iteration of i" , i ,"/" , j ,"/" , k
  END IF
  DO j = 0 , jend , 1
   DO k = 0 , kend , 1
    PRINT * ,"First loop body:" , i ,"/" , j ,"/" , k
   END DO
  END DO
  DO j = 2 , jend , 1
   DO k = 0 , kend , 1
    PRINT * ,"Second loop body:" , i ,"/" , j ,"/" , k
   END DO
  END DO
!$claw end loop-hoist
  DO j = 0 , jend , 1
   DO k = 0 , kend , 1
    IF ( i == 0 ) THEN
     PRINT * ,"GPU First iteration of i" , i ,"/" , j ,"/" , k
    END IF
    PRINT * ,"GPU First loop body:" , i ,"/" , j ,"/" , k
    IF ( j .ge. 2 ) THEN
     PRINT * ,"GPU Second loop body:" , i ,"/" , j ,"/" , k
    END IF
   END DO
  END DO
 END DO
END SUBROUTINE clawloop

