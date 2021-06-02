PROGRAM VECTOR_LOOP
  CALL claw
END

SUBROUTINE claw ( )
 INTEGER :: i = 10
 INTEGER :: vec1 ( 0 : 10 , 0 : 10 )
 INTEGER :: vec2 ( 0 : 10 , 0 : 10 )

 INTEGER :: claw_induction_0
 INTEGER :: claw_induction_1

 vec1 ( 0 : i , 0 : i ) = 0
 vec2 ( 0 : i , 0 : i ) = 100
!$claw expand target(cpu)
 vec1 ( 0 : i , 0 : i ) = vec2 ( 0 : i , 0 : i ) + 10
 DO claw_induction_0 = 0 , i
  DO claw_induction_1 = 0 , i
   vec1 ( claw_induction_0 , claw_induction_1 ) = vec2 ( claw_induction_0 ,&
    claw_induction_1 ) + 10
  END DO
 END DO
 PRINT * , vec1
 PRINT * , vec2
END SUBROUTINE claw
