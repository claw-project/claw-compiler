PROGRAM ifextract2
 INTEGER :: i
 LOGICAL :: test

 test = .TRUE.
 IF ( test ) THEN
  DO i = 1 , 10 , 1
   PRINT * ,"Then body:" , i
  END DO
 ELSE
  DO i = 1 , 10 , 1
   PRINT * ,"Else body:" , i
  END DO
 END IF
END PROGRAM ifextract2

