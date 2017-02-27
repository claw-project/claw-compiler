! Simple program to test the kcache directive
PROGRAM claw_test
  INTEGER :: istart = 0
  INTEGER :: iend = 10
  INTEGER :: jstart = 0
  INTEGER :: jend = 20
  CALL call_test(istart,iend,jstart,jend)
END PROGRAM claw_test

PURE FUNCTION f(i,j)
  INTEGER, INTENT(IN) :: i,j
  REAL :: f
  f = 1.0 * i * j
END FUNCTION

SUBROUTINE call_test(istart,iend,jstart,jend)
  INTEGER, INTENT(IN) :: istart, iend, jstart, jend
  INTEGER :: i,j
  REAL, DIMENSION(istart:iend,istart:iend) :: array6, array7

  DO i = istart, iend
    DO j = jstart, jend
      array6(i,j) = 1.0 * i * j
    END DO
  END DO

  DO i = istart, iend
    DO j = jstart, jend
      !$claw call array6=f(i,j) target(gpu)
      array7(i,j) = array6(i,j) * 2.0
    END DO
  END DO

  DO i = istart, iend
    DO j = jstart, jend
      !$claw call array6=f(i,j) target(cpu)
      array7(i,j) = array6(i,j) * 2.0
    END DO
  END DO
END SUBROUTINE call_test
