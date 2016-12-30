program mydirective_test
  integer :: i
  !$claw mydirective
  do i=1,10
    print *, 'First loop body:',i
  end do
end program mydirective_test
