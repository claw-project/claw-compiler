module mod11
!$claw
end module mod11

 module mod12
!$claw
end module mod12

  module mod13
!$claw
    use mod12
end module mod13

module mod_no_claw
    use mod11
end module mod_no_claw

   program p1
!$claw
    use mod12
    use mod13
end program p1
