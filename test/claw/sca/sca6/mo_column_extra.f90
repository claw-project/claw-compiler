!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column_extra
  IMPLICIT NONE
CONTAINS

  SUBROUTINE compute_one(nz, q, t)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only

    !$claw sca forward
    CALL compute_two(nz, q, t)
  END SUBROUTINE compute_one

  SUBROUTINE compute_two(nz, q, t)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only

    !$claw sca forward
    CALL compute_three(nz, q, t)
  END SUBROUTINE compute_two

  SUBROUTINE compute_three(nz, q, t)
    USE mo_column
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only

    !$claw sca forward
    CALL compute_solver(nz, q, t)
  END SUBROUTINE compute_three

END MODULE mo_column_extra
