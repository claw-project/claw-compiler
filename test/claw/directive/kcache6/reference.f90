MODULE kcache_module
 USE helper_module , ONLY: array1 , array2 , array3

CONTAINS
 SUBROUTINE kcache ( )

  INTEGER :: i
  INTEGER :: j
  REAL ( KIND= 8 ) :: array2_k
  REAL ( KIND= 8 ) :: array3_k
  REAL ( KIND= 8 ) :: array1_k_m1

  DO i = 1 , 10 , 1
   DO j = 1 , 20 , 1
    array1 ( i , j ) = 1.0
    array2 ( i , j ) = 2.0
    array3 ( i , j ) = 3.0
   END DO
  END DO
  DO i = 1 , 10 , 1
   DO j = 2 , 20 , 1
    array1_k_m1 = array1 ( i , j - 1 ) * 2.0
    array1 ( i , j ) = array1_k_m1
    array2_k = array2 ( i , j ) * 2.0 + array1_k_m1
    array2 ( i , j ) = array2_k
    array3_k = array3 ( i , j ) * 2.0 + array1_k_m1 + array2_k
    array3 ( i , j ) = array3_k
    array1_k_m1 = array2_k
   END DO
  END DO
  PRINT * , sum ( array1 )
  PRINT * , sum ( array2 )
  PRINT * , sum ( array3 )
 END SUBROUTINE kcache

END MODULE kcache_module

