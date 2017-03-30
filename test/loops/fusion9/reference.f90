PROGRAM test
 INTEGER :: i
 LOGICAL :: ldo

 ldo = .TRUE.
 IF ( ldo ) THEN
!$claw loop-fusion group(g1) constraint(none)
  DO i = 1 , 10 , 1
   PRINT * ,"Loop body #1"
   PRINT * ,"Loop body #2"
  END DO
 END IF
END PROGRAM test

