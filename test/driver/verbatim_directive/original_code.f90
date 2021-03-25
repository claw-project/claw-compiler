!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

PROGRAM testverbatim

  !$claw verbatim IF (.FALSE.) THEN
  PRINT*,'These lines'
  PRINT*,'are not printed'
  PRINT*,'if the the CLAW Compiler has processed'
  PRINT*,'the file.'
  !$claw verbatim END IF

END PROGRAM testverbatim
