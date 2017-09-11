PROGRAM array4_test

 CALL claw_test ( )
END PROGRAM array4_test

SUBROUTINE claw_test ( )
 INTEGER :: vec1 ( 0 : 10 )
 INTEGER :: vec2 ( 0 : 10 )
 INTEGER :: vec3 ( 0 : 10 )

 INTEGER :: claw_induction_0

 vec1 ( 0 : 10 ) = 0
 vec2 ( 0 : 10 ) = 100
 PRINT * , vec1
 PRINT * , vec2
 DO claw_induction_0 = 0 , 10
  vec1 ( claw_induction_0 ) = vec2 ( claw_induction_0 ) + 10
  vec2 ( claw_induction_0 ) = vec2 ( claw_induction_0 ) + 10
 END DO
 PRINT * , vec1
 PRINT * , vec2
 vec3 ( 0 : 10 ) = vec1 ( 0 : 10 ) + vec2 ( 0 : 10 )
 PRINT * , sum ( vec1 )
 PRINT * , sum ( vec2 )
END SUBROUTINE claw_test

