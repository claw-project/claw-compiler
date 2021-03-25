PROGRAM test
 INTEGER :: i
 INTEGER :: j

!$claw loop-fusion
 DO i = 1 , 10 , 1
  j = i
 END DO
 j = 20
!$claw loop-fusion
 DO i = 1 , 10 , 1
  IF ( j < 15 ) THEN
   j = i
   PRINT * ,"HERE"
  END IF
 END DO
END PROGRAM test

