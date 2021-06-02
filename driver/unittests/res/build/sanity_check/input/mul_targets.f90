module mod1
!$claw
end module mod1

module mod2
end module mod2

module mod3
    use mod2
end module mod3

program t1
    use mod2
    use mod3
end program t1

module t2
    use mod1
end module t2

module t3
    use mod1
    use mod2
end module t3
