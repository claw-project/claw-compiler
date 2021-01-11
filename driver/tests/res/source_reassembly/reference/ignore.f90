module broken_mod
    integer
end

PROGRAM p1

    use broken_mod
    integer :: x = 1, y = 2, z
    z = x + y
END PROGRAM p1

