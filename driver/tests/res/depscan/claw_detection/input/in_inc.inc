module mod21
!$claw
end module mod21

module mod22
end module mod22

module mod23
    use mod22
end module mod23

program p1
    use mod22
    use mod23
end program p1
