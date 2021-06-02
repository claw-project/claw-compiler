!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

PROGRAM testmacro

#ifdef _OPENMP
  PRINT*,'These lines'
  PRINT*,'are not ignored'
  PRINT*,'by the preprocessor.'
#else
  PRINT*,'These lines'
  PRINT*,'are ignored by the preprocessor.'
#endif

END PROGRAM testmacro
