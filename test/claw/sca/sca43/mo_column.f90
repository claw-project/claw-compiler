!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE
CONTAINS

  ! Compute single point with elemental function
  ELEMENTAL FUNCTION compute_point(t, w) RESULT(q)
    IMPLICIT NONE

    !$claw model-data
    REAL, INTENT(IN) :: t ! Field declared as a single point only
    REAL, OPTIONAL, INTENT(IN) :: w
    REAL :: q             ! Field declared as a single point only
    !$claw end model-data

    REAL :: c

    ! The following directive is optional in ELEMENTAL function/subroutine

    !$claw sca

    c = 5.345
    q = q + t * c

    if(present(w)) then
      q = q + w
    end if

  END FUNCTION compute_point
END MODULE mo_column
