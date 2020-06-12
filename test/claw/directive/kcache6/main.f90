!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Entry point for the module test
!
PROGRAM claw_test
  USE kcache_module, ONLY: kcache
    
  CALL kcache()
END PROGRAM claw_test
