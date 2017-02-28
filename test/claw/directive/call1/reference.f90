PROGRAM claw_test
 INTEGER :: istart = 0
 INTEGER :: iend = 10
 INTEGER :: jstart = 0
 INTEGER :: jend = 20

 CALL call_test ( istart , iend , jstart , jend )
END PROGRAM claw_test

SUBROUTINE call_test ( istart , iend , jstart , jend )
 INTEGER , INTENT(IN) :: istart
 INTEGER , INTENT(IN) :: iend
 INTEGER , INTENT(IN) :: jstart
 INTEGER , INTENT(IN) :: jend
 INTEGER :: i
 INTEGER :: j
 REAL :: array6 ( istart : iend , istart : iend )
 REAL :: array7 ( istart : iend , istart : iend )

 DO i = istart , iend , 1
  DO j = jstart , jend , 1
   array6 ( i , j ) = 1.0 * i * j
  END DO
 END DO
 DO i = istart , iend , 1
  DO j = jstart , jend , 1
   array7 ( i , j ) = f ( i , j ) * 2.0
  END DO
 END DO
END SUBROUTINE call_test

PURE FUNCTION f ( i , j )
 INTEGER , INTENT(IN) :: i
 INTEGER , INTENT(IN) :: j
 REAL :: f

 f = 1.0 * i * j
END FUNCTION f

