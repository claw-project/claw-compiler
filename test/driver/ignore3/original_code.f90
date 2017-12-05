!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

PROGRAM testignore

  !$claw ignore
  REAL :: real_var_1, &
          real_var_2, &
          real_var_3, &
          real_var_4, &
          real_var_5, &
          real_var_6, &
          real_var_7, &
          real_var_8, &
          real_var_9, &
          real_var_10, &
          real_var_11, &
          real_var_12, &
          real_var_13
  !$claw end ignore

END PROGRAM testignore
