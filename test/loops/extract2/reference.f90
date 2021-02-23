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
  DO i = istart , iend , 1
   CALL clawloop_extracted0 ( value1 ( i ) , value2 ( i ) )
  END DO
  CALL test ( )
 END SUBROUTINE loop_extract

 SUBROUTINE clawloop ( value1 , value2 )

  INTEGER :: value1 ( 1 : 10 )
  INTEGER :: value2 ( 1 : 10 )
  INTEGER :: i
  INTEGER :: istart = 1
  INTEGER :: iend = 10

  DO i = istart , iend , 1
   PRINT * ,"value1: " , value1 ( i )
   PRINT * ,"value2: " , value2 ( i )
  END DO
 END SUBROUTINE clawloop

 SUBROUTINE clawloop_extracted1 ( value1 , value2 )

  INTEGER :: value1
  INTEGER :: value2
  INTEGER :: i
  INTEGER :: istart = 1
  INTEGER :: iend = 10

  PRINT * ,"value1: " , value1
  PRINT * ,"value2: " , value2
 END SUBROUTINE clawloop_extracted1

 SUBROUTINE clawloop_extracted0 ( value1 , value2 )

  INTEGER :: value1
  INTEGER :: value2
  INTEGER :: i
  INTEGER :: istart = 1
  INTEGER :: iend = 10

  PRINT * ,"value1: " , value1
  PRINT * ,"value2: " , value2
 END SUBROUTINE clawloop_extracted0

 SUBROUTINE test ( )

  INTEGER :: v1 ( 1 : 10 )
  INTEGER :: v2 ( 1 : 10 )
  INTEGER :: j
  INTEGER :: i
  INTEGER :: istart = 1
  INTEGER :: iend = 10

  DO j = 1 , 10 , 1
   v1 ( j ) = j
   v2 ( j ) = j
  END DO
  DO i = istart , iend , 1
   CALL clawloop_extracted1 ( v1 ( i ) , v2 ( i ) )
  END DO
 END SUBROUTINE test

END MODULE mod

program main
  use mod

  call LOOP_EXTRACT()
end program main
