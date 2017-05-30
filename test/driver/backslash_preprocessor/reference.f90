MODULE mod1
 INTEGER , PARAMETER :: p1 = 6
 INTEGER , PARAMETER :: p2 = 37
 INTEGER , PARAMETER :: p3 = 12
 INTEGER , PARAMETER :: p4 = 307
 INTEGER , PARAMETER :: sp = selected_real_kind ( 6 , 37 )
 INTEGER , PARAMETER :: dp = selected_real_kind ( 12 , 307 )
 INTEGER , PARAMETER :: wp = selected_real_kind ( 12 , 307 )
 INTEGER , PARAMETER :: vp = selected_real_kind ( 12 , 307 )
 INTEGER , PARAMETER :: vp2 = selected_real_kind ( 12 , 307 )
 INTEGER , PARAMETER :: pi2 = 4
 INTEGER , PARAMETER :: pi4 = 9
 INTEGER , PARAMETER :: pi8 = 14
 INTEGER , PARAMETER :: i2 = selected_int_kind ( 4 )
 INTEGER , PARAMETER :: i4 = selected_int_kind ( 9 )
 INTEGER , PARAMETER :: i8 = selected_int_kind ( 14 )
END MODULE mod1

