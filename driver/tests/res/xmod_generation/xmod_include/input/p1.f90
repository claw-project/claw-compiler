module m3
    use mod1
    use mod2
end

program p1
    use m3
!$claw
end program p1
