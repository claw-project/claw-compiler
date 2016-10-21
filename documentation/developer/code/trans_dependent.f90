PROGRAM loop_fusion
  INTEGER :: i

  !$claw loop-fusion
  DO i = 1, 10
    ! Loop body
  END DO

  !$claw loop-fusion
  DO i = 1, 10
    ! Loop body
  END DO
END PROGRAM loop_fusion
