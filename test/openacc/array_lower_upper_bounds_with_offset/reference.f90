PROGRAM VECTOR_LOOP
  CALL claw
END

SUBROUTINE claw ( )

 INTEGER :: upper = 10
 INTEGER :: lower = 0
 INTEGER :: vec1 ( 0 : 10 )
 INTEGER :: vec2 ( 0 : 10 )
 INTEGER :: claw_induction_0
 INTEGER :: claw_induction_1

 vec1 = 0
 vec2 = 100
 DO claw_induction_1 = lower , upper
  vec1 ( claw_induction_1 ) = vec2 ( claw_induction_1 ) + 10
 END DO
 DO claw_induction_0 = lower + 1 , upper
  vec2 ( claw_induction_0 ) = vec2 ( 1 ) + 10
 END DO
 PRINT * , vec1
 PRINT * , vec2
END SUBROUTINE claw

