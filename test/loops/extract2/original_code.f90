!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the loop-extract directive
!

PROGRAM LOOP_EXTRACT
  INTEGER :: value1(10)
  INTEGER :: value2(10)
  INTEGER :: j
  DO j=1,10
    value1(j) = j
    value2(j) = j
  END DO

  !$claw loop-extract range(i=istart,iend) map(value1:i) map(value2:i)
  CALL clawloop(value1, value2)

  CALL test

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

END SUBROUTINE clawloop


SUBROUTINE test
  INTEGER :: v1(10)
  INTEGER :: v2(10)

  INTEGER :: j
  DO j=1,10
    v1(j) = j
    v2(j) = j
  END DO

  !$claw loop-extract range(i=istart,iend) map(v1/value1:i) map(v2/value2:i)
  CALL clawloop(v1, v2)
END SUBROUTINE test
