PROGRAM vector_loop

 CALL claw ( )
 CALL claw_transformed ( )
END PROGRAM vector_loop

SUBROUTINE claw ( )

 INTEGER :: j
 INTEGER :: vec1 ( 0 : 10 )
 INTEGER :: claw_induction_0

 DO j = 0 , 10 , 1
  vec1 ( j ) = j
 END DO
!$acc parallel
!$acc loop
 DO claw_induction_0 = 1 , size ( vec1 , 1 )
  vec1 ( claw_induction_0 ) = vec1 ( claw_induction_0 ) + 10
 END DO
!$acc end parallel
 PRINT * , vec1
END SUBROUTINE claw

SUBROUTINE claw_transformed ( )
 INTEGER :: j

 INTEGER :: i = 10
 INTEGER :: vec1 ( 0 : 10 )
 INTEGER :: claw_induc1

 DO j = 0 , i , 1
  vec1 ( j ) = j
 END DO
 DO claw_induc1 = 0 , i , 1
  vec1 ( claw_induc1 ) = vec1 ( claw_induc1 ) + 10
 END DO
 PRINT * , vec1
END SUBROUTINE claw_transformed

