PROGRAM array4_test
  CALL claw_test
END

SUBROUTINE claw_test ( )

 INTEGER :: vec1 ( 1 : 10 )
 INTEGER :: vec2 ( 1 : 10 )
 INTEGER :: vec3 ( 1 : 10 )
 INTEGER :: claw_induction_0

 vec1 ( : ) = 0
 vec2 ( : ) = 100
 PRINT * , vec1
 PRINT * , vec2
 DO claw_induction_0 = 1 , size ( vec1 , 1 )
  vec1 ( claw_induction_0 ) = vec2 ( claw_induction_0 ) + 10
  vec2 ( claw_induction_0 ) = vec2 ( claw_induction_0 ) + 10
 END DO
 PRINT * , vec1
 PRINT * , vec2
 vec3 ( : ) = vec1 ( : ) + vec2 ( : )
 PRINT * , sum ( vec1 )
 PRINT * , sum ( vec2 )
END SUBROUTINE claw_test

