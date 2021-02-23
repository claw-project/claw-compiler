PROGRAM array4_test
  CALL claw_test
END

SUBROUTINE claw_test ( )
 INTEGER , ALLOCATABLE :: vec1 ( : , : )
 INTEGER , ALLOCATABLE :: vec2 ( : , : )

 INTEGER :: claw_induction_0
 INTEGER :: claw_induction_1

 ALLOCATE ( vec1 ( 10 , 20 ) )
 ALLOCATE ( vec2 ( 10 , 20 ) )
 vec1 ( : , : ) = 0
 vec2 ( : , : ) = 100
 DO claw_induction_0 = 1 , size ( vec1 , 1 )
  DO claw_induction_1 = 1 , size ( vec1 , 2 )
   vec1 ( claw_induction_0 , claw_induction_1 ) = vec2 ( claw_induction_0 ,&
    claw_induction_1 ) + 10
   vec2 ( claw_induction_0 , claw_induction_1 ) = vec2 ( claw_induction_0 ,&
    claw_induction_1 ) + 10
  END DO
 END DO
 PRINT * , sum ( vec1 )
 PRINT * , sum ( vec2 )
 DEALLOCATE ( vec1 )
 DEALLOCATE ( vec2 )
END SUBROUTINE claw_test

