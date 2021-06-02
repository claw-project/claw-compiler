PROGRAM claw_test
  CALL claw_hoist2
END PROGRAM claw_test

SUBROUTINE claw_hoist2 ( )

 INTEGER :: jt
 INTEGER :: i
 INTEGER :: j
 INTEGER :: ntrac
 INTEGER :: klev
 INTEGER :: kproma
 REAL :: var1
 REAL :: var2
 REAL :: var3
 REAL :: var4
 REAL :: var5
 REAL :: time_step_len
 REAL :: zsedtend
 REAL :: array2d_1 ( 1 : 100 )
 REAL :: array2d_2
 REAL :: array2d_3 ( 1 : 10 , 1 : 100 )
 REAL :: array2d_4 ( 1 : 10 , 1 : 100 )
 REAL :: array2d_5 ( 1 : 10 , 1 : 100 )
 REAL :: array3d_1 ( 1 : 10 , 1 : 100 , 1 : 2 )
 REAL :: array3d_2 ( 1 : 10 , 1 : 100 , 1 : 2 )
 TYPE :: dummy
  REAL :: value1
  REAL :: value2
 END TYPE dummy
 TYPE ( dummy ) :: tdum

 zsedtend = 1.0
 var3 = 2.0
 ntrac = 2
 klev = 10
 kproma = 100
 array2d_1 ( : ) = 0.0
 array2d_2 = 0.0
 array2d_3 ( : , : ) = 0.0
 array2d_4 ( : , : ) = 0.0
 array2d_5 ( : , : ) = 0.0
!$ACC parallel loop gang vector collapse(2)
 DO jt = 1 , ntrac , 1
  DO i = 1 , kproma , 1
   DO j = 1 , klev , 1
    IF ( .FALSE. ) THEN
     CYCLE
    END IF
    var1 = tdum % value1
    var2 = var1 ** ( 2. )
    IF ( jt == 2 ) THEN
     array2d_2 = min ( tdum % value2 * 2 , 50.e-6 )
    END IF
    array2d_1 ( j ) = 0.0
    IF ( array2d_2 > 0.0 ) THEN
     var3 = array3d_1 ( i , j , jt ) + array3d_2 ( i , j , jt ) *&
      time_step_len
     var5 = min ( 2.0 / 9.0 , array2d_1 ( j ) )
     zsedtend = min ( max ( 0.0 , var3 * var5 / array2d_3 ( i , j ) ) , 2.0 )
     array3d_2 ( i , j , jt ) = array3d_2 ( i , j , jt ) - var4
     array2d_1 ( j ) = var4 * array2d_4 ( i , j )
    END IF
    IF ( j .ge. 2 ) THEN
     array3d_2 ( i , j , jt ) = array3d_2 ( i , j , jt ) + array2d_1 ( j - 1 )&
      / array2d_5 ( i , j )
    END IF
   END DO
  END DO
  DO j = 2 , klev , 1
   DO i = 1 , kproma , 1
    array3d_2 ( i , j , jt ) = array3d_2 ( i , j , jt ) + 5.0 / array2d_5 ( i&
     , j )
   END DO
  END DO
 END DO
!$ACC end parallel
END SUBROUTINE claw_hoist2

