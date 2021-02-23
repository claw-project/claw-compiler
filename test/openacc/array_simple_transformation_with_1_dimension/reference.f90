PROGRAM VECTOR_LOOP
  CALL claw
  CALL claw_transformed
END

SUBROUTINE claw ( )

 INTEGER :: j
 INTEGER :: vec1 ( 0 : 10 )
 INTEGER :: claw_induction_0

 DO j = 0 , 10 , 1
  vec1 ( j ) = j
 END DO
!$acc data present(vec1(:))
!$acc parallel
!$acc loop gang vector
 DO claw_induction_0 = 1 , size ( vec1 , 1 )
  vec1 ( claw_induction_0 ) = vec1 ( claw_induction_0 ) + 10
 END DO
!$acc end parallel
!$acc end data
 PRINT * , vec1
END SUBROUTINE claw

SUBROUTINE claw_transformed
  INTEGER :: i = 10
  INTEGER, DIMENSION(0:10) :: vec1
  INTEGER :: claw_induc1 ! induction variable is declared

  DO j = 0, i
    vec1(j) = j
  END DO

  ! Do stmt is inserted
  DO claw_induc1=0,i
    ! index range are replaced with arrayIndex
    vec1(claw_induc1) = vec1(claw_induc1) + 10
  END DO

  PRINT*,vec1
END SUBROUTINE claw_transformed

