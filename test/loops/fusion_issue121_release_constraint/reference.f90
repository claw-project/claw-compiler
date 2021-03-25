PROGRAM test
 INTEGER :: i
 LOGICAL :: ldo

 ldo = .TRUE.
 DO i = 1 , 10 , 1
  PRINT * ,"Loop body #1"
  PRINT * ,"Loop body #2"
 END DO
 IF ( ldo ) THEN
  PRINT * ,"I did"
 END IF
END PROGRAM test

