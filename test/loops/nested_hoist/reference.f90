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
  DO j = 0 , jend , 1
   DO k = 0 , kend , 1
    IF ( i == 0 ) THEN
     PRINT * ,"First iteration of i" , i ,"/" , j ,"/" , k
    END IF
    PRINT * ,"First loop body:" , i ,"/" , j ,"/" , k
    IF ( j .ge. 2 ) THEN
     PRINT * ,"Second loop body:" , i ,"/" , j ,"/" , k
    END IF
   END DO
  END DO
 END DO
END SUBROUTINE clawloop

