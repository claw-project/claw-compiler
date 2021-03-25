module mod11
end module mod11

module mod12
    use mod11
end module mod12

module mod13
    integer ! Error on purpose
end module mod13

program p1
    use mod12
    !$claw ignore
    use mod13
    !$claw end ignore
end program p1
