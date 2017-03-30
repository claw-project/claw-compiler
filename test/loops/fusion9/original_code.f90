program test
  integer :: i
  logical :: ldo

  ldo = .true.

  if ( ldo ) then
    !$claw loop-fusion group(g1) constraint(none)
    do i = 1, 10
      print*,'Loop body #1'
    end do
  end if

  !$claw loop-fusion group(g1) constraint(none)
  do i = 1, 10
    print*,'Loop body #2'
  end do

end program test
