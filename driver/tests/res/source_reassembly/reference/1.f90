MODULE mod11
    implicit none
END MODULE mod11

MODULE mod12
    implicit none
END MODULE mod12

MODULE mod13
 USE mod12
integer, parameter x = 2
END MODULE mod13

module mod_no_claw
    use mod11
end module mod_no_claw

PROGRAM p1
 USE mod12
 USE mod13
 INTEGER :: x = 1
 INTEGER :: y = 2
 INTEGER :: z

 z = ( x + y )
END PROGRAM p1

