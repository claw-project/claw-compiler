PROGRAM testignore



!$claw remove
  print*,"These lines"
  print*,"are ignored"
  print*,"by the CLAW compiler"
  print*,"but kept in the final transformed code"
  print*,"with the remove directives."
!$claw end remove


  print*,"These lines"
  print*,"are ignored"

END PROGRAM testignore

