PROGRAM claw_test
 INTEGER :: istart = 0
 INTEGER :: iend = 10
 INTEGER :: jstart = 0
 INTEGER :: jend = 20

 CALL kcache ( istart , iend , jstart , jend )
END PROGRAM claw_test

SUBROUTINE kcache ( istart , iend , jstart , jend )
 INTEGER , INTENT(IN) :: istart
 INTEGER , INTENT(IN) :: iend
 INTEGER , INTENT(IN) :: jstart
 INTEGER , INTENT(IN) :: jend
 INTEGER :: i
 INTEGER :: j
 REAL ( KIND= 8 ) :: array6 ( istart : iend , istart : iend )
 REAL ( KIND= 8 ) :: array7 ( istart : iend , istart : iend )
 REAL ( KIND= 8 ) :: array8 ( istart : iend , istart : iend )
 REAL ( KIND= 8 ) :: array9 ( istart : iend , istart : iend )
 REAL ( KIND= 8 ) :: array10 ( istart : iend , istart : iend )
 REAL ( KIND= 8 ) :: array8_k_m1
 REAL ( KIND= 8 ) :: array9_k_m1

 DO i = istart , iend , 1
  array6 ( i , 1 ) = 1.0
  array7 ( i , 1 ) = 2.0
  array8 ( i , 1 ) = 3.0
  array9 ( i , 1 ) = 4.0
  array10 ( i , 1 ) = 4.0
 END DO
 DO i = istart , iend , 1
  DO j = jstart + 1 , jend , 1
!$claw kcache data(array6, array7) offset(0,-1) target(cpu)
   array6 ( i , j ) = array6 ( i , j ) * 2.0
   array7 ( i , j ) = array7 ( i , j ) * 2.0 + array6 ( i , j - 1 )
   array8_k_m1 = array8 ( i , j ) * 2.0 + array6 ( i , j - 1 ) + array7 ( i ,&
    j - 1 )
   array8 ( i , j ) = array8_k_m1
   array9_k_m1 = array9 ( i , j ) * 2.0 + array6 ( i , j - 1 ) + array8_k_m1
   array9 ( i , j ) = array9_k_m1
   array10 ( i , j ) = array9_k_m1 + 1.0
  END DO
 END DO
 PRINT * , sum ( array6 )
 PRINT * , sum ( array7 )
 PRINT * , sum ( array8 )
 PRINT * , sum ( array9 )
END SUBROUTINE kcache

