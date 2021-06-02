module mod1
!$claw
end module mod1

module mod2
end module mod2

module mod3
    use mod2
end module mod3

program p1
    use mod2
    use mod_undefined
end program p1
