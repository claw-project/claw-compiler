!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Small test case to test macro passing to the preprocessor by the CLAW driver
!

program testmacro

#ifdef USER_MACRO1
  print*, 'These lines'
  print*, 'are not ignored'
  print*, 'by the preprocessor.'
#else
  print*, 'These lines'
  print*, 'are ignored by the preprocessor.'
#endif

#ifdef USER_MACRO2
  print*, 'This is in.'
#endif

end program testmacro
