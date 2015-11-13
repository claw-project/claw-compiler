! Simple program to test the loop-extract directive

PROGRAM LOOP_EXTRACT
  INTEGER :: value1(10)
  INTEGER :: value2(10)


  !$claw loop-extract range(i=istart,iend) map(value1:i) map(value2:i)
  CALL clawloop(value1, value2)
END

SUBROUTINE clawloop(value1, value2)
  INTEGER :: value1(10)
  INTEGER :: value2(10)
  INTEGER :: i
  INTEGER :: istart=1
  INTEGER :: iend=10

  DO i=istart,iend
    print*,'value1: ',value1(i)
    print*,'value2: ',value2(i)
  END DO

END
