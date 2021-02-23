PROGRAM remove1
  CALL dummy
END PROGRAM remove1

SUBROUTINE dummy ( )
 INTEGER :: k
 INTEGER :: i
 INTEGER :: kend = 5
 INTEGER :: iend = 10


 DO k = 1 , kend , 1
  DO i = 1 , iend , 1
  END DO
  DO i = 1 , iend , 1
  END DO
 END DO
END SUBROUTINE dummy

