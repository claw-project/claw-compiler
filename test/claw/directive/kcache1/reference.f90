PROGRAM claw_test
 INTEGER :: istart = 0
 INTEGER :: iend = 10
 INTEGER :: jstart = 1
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
 REAL ( KIND= 8 ) :: array6_k__
 REAL ( KIND= 8 ) :: array7_k__
 REAL ( KIND= 8 ) :: array8_k__
 REAL ( KIND= 8 ) :: array9_k__

 DO i = istart , iend , 1
  array6 ( i , jstart ) = 1.0
  array7 ( i , jstart ) = 2.0
  array8 ( i , jstart ) = 3.0
  array9 ( i , jstart ) = 4.0
  array10 ( i , jstart ) = 4.0
 END DO
 DO i = istart , iend , 1
  DO j = jstart + 1 , jend , 1
   array6_k__ = array6 ( i , j - 1 ) * 2.0
   array6 ( i , j ) = array6_k__
   array7_k__ = array7 ( i , j - 1 ) * 2.0 + array6_k__
   array7 ( i , j ) = array7_k__
   array8_k__ = array8 ( i , j - 1 ) * 2.0 + array6_k__ + array7_k__
   array8 ( i , j ) = array8_k__
   array9_k__ = array9 ( i , j - 1 ) * 2.0 + array6_k__ + array8_k__
   array9 ( i , j ) = array9_k__
   array10 ( i , j ) = array9_k__ + 1.0
  END DO
 END DO
 PRINT * , sum ( array6 )
 PRINT * , sum ( array7 )
 PRINT * , sum ( array8 )
 PRINT * , sum ( array9 )
END SUBROUTINE kcache

