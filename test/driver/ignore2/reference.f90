PROGRAM testignore


  !$claw remove
  PRINT*,'These lines'
  PRINT*,'are ignored'
  PRINT*,'by the CLAW Compiler'
  PRINT*,'but kept in the final transformed code'
  PRINT*,'with the remove directives.'
  !$claw end remove


  PRINT*,'These lines'
  PRINT*,'are ignored'

END PROGRAM testignore

