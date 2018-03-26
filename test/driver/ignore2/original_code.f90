!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

PROGRAM testignore

  !$claw ignore
  !$claw remove
  PRINT*,'These lines'
  PRINT*,'are ignored'
  PRINT*,'by the CLAW Compiler'
  PRINT*,'but kept in the final transformed code'
  PRINT*,'with the remove directives.'
  !$claw end remove
  !$claw end ignore

  !$claw ignore
  PRINT*,'These lines'
  PRINT*,'are ignored'
  !$claw end ignore

  !$claw remove
  PRINT*,'These lines'
  PRINT*,'are not ignored.'
  !$claw end remove

END PROGRAM testignore
