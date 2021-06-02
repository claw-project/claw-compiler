module mod21
!$claw ignore
    implicit none
!$claw end ignore
end module mod21


 module mod22
!$claw ignore
    implicit none
!$claw end ignore
end module mod22


  module mod23
    use mod22
!$claw verbatim integer, parameter x = 2
end module mod23


module mod_no_claw2
    use mod21
end module mod_no_claw2


   program p2
    use mod22
    use mod23
!$claw ignore
!$claw end ignore
    integer :: x = 1, y = 2, z
    z = x + y
end program p2
