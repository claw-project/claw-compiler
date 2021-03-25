!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

program test
  integer :: i, j
  !$claw loop-fusion
  do i = 1, 10
    j = i
  end do

  j = 20

  !$claw loop-fusion
  do i = 1, 10
    if ( j < 15 ) then
      j = i
      print*,'HERE'
    end if
  end do
end program test
