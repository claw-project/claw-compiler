PROGRAM array4_test
  CALL claw_test
END

SUBROUTINE claw_test ( )

 INTEGER :: vec1 ( 0 : 10 )
 INTEGER :: vec2 ( 0 : 10 )
 INTEGER :: vec3 ( 0 : 10 )
 INTEGER :: vec4 ( 0 : 20 )
 INTEGER :: vec5 ( 0 : 20 )
 INTEGER :: claw_induction_0

 vec1 ( : ) = 0
 vec2 ( : ) = 100
 vec4 ( : ) = 10
 vec5 ( : ) = 11
!$acc data present(vec5(:),vec4(:),vec1(:),vec2(:))
!$acc parallel
!$acc loop gang vector
 DO claw_induction_0 = 1 , size ( vec1 , 1 )
  vec1 ( claw_induction_0 ) = vec2 ( claw_induction_0 ) + 10
  vec2 ( claw_induction_0 ) = vec1 ( claw_induction_0 ) + 10
  vec4 ( claw_induction_0 ) = vec5 ( claw_induction_0 ) + 1
  vec4 ( claw_induction_0 ) = vec4 ( claw_induction_0 ) + 1
 END DO
!$acc end parallel
!$acc end data
 vec3 ( : ) = vec1 ( : ) + vec2 ( : )
 PRINT * , vec1
 PRINT * , vec2
END SUBROUTINE claw_test

