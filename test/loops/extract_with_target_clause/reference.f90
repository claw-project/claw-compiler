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
!$claw loop-extract range(i=istart,iend) map(value1:i) map(value2:i) target(cpu)
  CALL clawloop ( value1 , value2 )
  DO i = istart , iend , 1
   CALL clawloop_extracted0 ( value1 ( i ) , value2 ( i ) )
  END DO
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

 SUBROUTINE clawloop_extracted0 ( value1 , value2 )

  INTEGER :: value1
  INTEGER :: value2
  INTEGER :: i
  INTEGER :: istart = 1
  INTEGER :: iend = 10

  PRINT * ,"value1: " , value1
  PRINT * ,"value2: " , value2
 END SUBROUTINE clawloop_extracted0

END MODULE mod

program main
  use mod

  call LOOP_EXTRACT()
end program main
