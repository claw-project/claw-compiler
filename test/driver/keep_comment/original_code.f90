!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

program keep_comment
  ! User comment should be preseved in transformed code.
  print*, 'Dummy program to test driver options' ! EOL comment not-preseved
end program keep_comment
