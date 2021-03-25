
module m3
    integer !This is to produce an error
end module m3

module m2
    use m3
end module m2

module m1
    use m2
end module m1

program p1
    use m1
!$claw
end program p1
