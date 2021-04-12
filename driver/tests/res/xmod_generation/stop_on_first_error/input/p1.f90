
module m3
end module m3

module m2
end module m2

module m1
    integer !This is to produce an error
end module m1

program p1
    use m1
    use m2
    use m3
!$claw
end program p1
