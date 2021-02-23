PROGRAM remove4
  CALL dummy
END PROGRAM remove4

SUBROUTINE dummy ( )
 INTEGER :: k
 INTEGER :: i
 INTEGER :: z
 INTEGER :: kend = 5
 INTEGER :: iend = 10


 DO k = 1 , kend , 1
  DO i = 1 , iend , 1
  END DO
!$claw remove target(cpu)
  z = k + 1
  z = z ** ( 2 )
!$claw end remove
 END DO
END SUBROUTINE dummy

