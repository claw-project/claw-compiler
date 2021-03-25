MODULE mod

CONTAINS
 SUBROUTINE loop_extract ( )

  INTEGER :: value1 ( 1 : 10 )
  INTEGER :: value2 ( 1 : 10 )
  INTEGER :: j
  INTEGER :: i
  INTEGER :: istart = 1
  INTEGER :: iend = 10

  DO j = 1 , 10 , 1
   value1 ( j ) = j
   value2 ( j ) = j
  END DO
!$acc parallel
!$acc loop seq
  DO i = istart , iend , 1
   CALL clawloop_extracted0 ( value1 ( i ) , value2 ( i ) )
  END DO
!$acc end parallel
 END SUBROUTINE loop_extract

 SUBROUTINE clawloop ( value1 , value2 )

  INTEGER :: value1 ( 1 : 10 )
  INTEGER :: value2 ( 1 : 10 )
  INTEGER :: i
  INTEGER :: j
  INTEGER :: k
  INTEGER :: istart = 1
  INTEGER :: iend = 10

  DO j = 1 , 10 , 2
   PRINT * ,"j" , j
  END DO
  DO i = istart , iend , 1
   PRINT * ,"value1: " , value1 ( i )
   PRINT * ,"value2: " , value2 ( i )
  END DO
  DO k = 1 , 10 , 3
   PRINT * ,"k" , k
  END DO
 END SUBROUTINE clawloop

 SUBROUTINE clawloop_extracted0 ( value1 , value2 )

  INTEGER :: value1
  INTEGER :: value2
  INTEGER :: i
  INTEGER :: j
  INTEGER :: k
  INTEGER :: istart = 1
  INTEGER :: iend = 10

  DO j = 1 , 10 , 2
   PRINT * ,"j" , j
  END DO
  PRINT * ,"value1: " , value1
  PRINT * ,"value2: " , value2
  DO k = 1 , 10 , 3
   PRINT * ,"k" , k
  END DO
 END SUBROUTINE clawloop_extracted0

END MODULE mod

program main
  use mod

  call LOOP_EXTRACT()
end program main
