!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

program ecmwf_case1
  integer :: jl, jm
  !$claw acc kernels
  !$claw acc loop
  !$claw loop-fusion group(jl) constraint(none)
  DO JL=1,10
    print*,'1st jl loop body'
  END DO

  !$claw acc loop
  !$claw loop-hoist(JL) fusion group(jl) constraint(none)
  !$claw acc loop
  DO JM=1,10
    IF (.TRUE.) THEN
      DO JL=1,10
        print*,'2nd jm/jl loop body'
      END DO
    END IF
  END DO
  !$claw end loop-hoist

  !$claw loop-fusion group(g3)
  DO JM=1,10
    print*,'3rd jm loop body'
  END DO

  !$claw acc loop
  !$claw loop-hoist(JL) fusion group(jl) constraint(none)
  !$claw acc loop
  DO JM=1,10
    IF (.TRUE.) THEN
      DO JL=1,10
        print*,'4th jm/jl loop body'
      END DO
    END IF
  END DO
  !$claw end loop-hoist

  !$claw loop-fusion group(jl) constraint(none)
  DO JL=1,10
    print*,'5th jl loop body'
  END DO
  !$claw acc end kernels
end program ecmwf_case1
