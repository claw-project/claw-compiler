module broken_mod
    integer
end

program p1
!$claw ignore
    use broken_mod
    integer :: x = 1, y = 2, z
    z = x + y
!$claw end ignore
end program p1
