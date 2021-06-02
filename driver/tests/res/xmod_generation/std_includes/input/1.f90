module mod1
    use ieee_arithmetic
    use ieee_exceptions
    use ieee_features
    use openacc
    use iso_c_binding
    use iso_fortran_env
end module mod1

program p1
    use mod1
!$claw
end program p1
