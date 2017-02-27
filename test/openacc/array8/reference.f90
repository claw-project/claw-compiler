PROGRAM array4_test


 CALL claw_test ( )
END PROGRAM array4_test

SUBROUTINE claw_test ( )
 INTEGER :: vec1 ( 0 : 10 )
 INTEGER :: vec2 ( 0 : 10 )
 INTEGER :: vec3 ( 0 : 10 )
 INTEGER :: vec4 ( 0 : 20 )
 INTEGER :: vec5 ( 0 : 20 )

 INTEGER :: claw_induction_0
 INTEGER :: claw_induction_1


 vec1 ( 0 : 10 ) = 0
 vec2 ( 0 : 10 ) = 100
 vec4 ( 0 : 20 ) = 10
 vec5 ( 0 : 20 ) = 11
!$acc parallel
!$acc loop vector
 DO claw_induction_0 = 0 , 10
  vec1 ( claw_induction_0 ) = vec2 ( claw_induction_0 ) + 10
  vec2 ( claw_induction_0 ) = vec1 ( claw_induction_0 ) + 10
 END DO
!$acc end parallel
!$acc parallel
!$acc loop vector
 DO claw_induction_1 = 0 , 20
  vec4 ( claw_induction_1 ) = vec5 ( claw_induction_1 ) + 1
  vec4 ( claw_induction_1 ) = vec4 ( claw_induction_1 ) + 1
 END DO
!$acc end parallel
 vec3 ( 0 : 10 ) = vec1 ( 0 : 10 ) + vec2 ( 0 : 10 )
 PRINT * , vec1
 PRINT * , vec2
END SUBROUTINE claw_test

