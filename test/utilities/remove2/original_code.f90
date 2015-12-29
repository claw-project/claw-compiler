! Simple program to test the remove directive

PROGRAM remove1
  CALL dummy
END PROGRAM remove1

SUBROUTINE dummy
  INTEGER :: k,i
  INTEGER :: kend = 5, iend = 10
  DO k=1, kend
    DO i=1, iend

    END DO

    !$claw remove
    PRINT*, k
    PRINT*, k+1
    !$claw end remove

    DO i=1, iend
      ! loop #2 body here
    END DO
  END DO

END SUBROUTINE dummy
