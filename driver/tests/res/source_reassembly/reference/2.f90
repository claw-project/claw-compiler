MODULE mod21
    implicit none
END MODULE mod21

MODULE mod22
    implicit none
END MODULE mod22

MODULE mod23
 USE mod22
integer, parameter x = 2
END MODULE mod23

module mod_no_claw2
    use mod21
end module mod_no_claw2

PROGRAM p2
 USE mod22
 USE mod23
 INTEGER :: x = 1
 INTEGER :: y = 2
 INTEGER :: z

 z = ( x + y )
END PROGRAM p2

