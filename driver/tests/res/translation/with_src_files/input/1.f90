module mod11
!$claw ignore
    implicit none
!$claw end ignore
end module mod11


 module mod12
!$claw ignore
    implicit none
!$claw end ignore
end module mod12


  module mod13
    use mod12
!$claw verbatim integer, parameter x = 2
end module mod13


module mod_no_claw
    use mod11
end module mod_no_claw


   program p1
    use mod12
    use mod13
!$claw ignore
    integer :: pi
!$claw end ignore
end program p1
