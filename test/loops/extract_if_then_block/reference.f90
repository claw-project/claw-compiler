PROGRAM ifextract1
 INTEGER :: i
 LOGICAL :: test

 test = .TRUE.
 IF ( test ) THEN
  DO i = 1 , 10 , 1
   PRINT * ,"First loop body:" , i
  END DO
 END IF
END PROGRAM ifextract1

