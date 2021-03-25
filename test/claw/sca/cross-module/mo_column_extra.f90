!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column_extra
  IMPLICIT NONE
CONTAINS

  SUBROUTINE compute(nz, q, t)
    USE mo_column
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only

    !$claw sca forward
    CALL compute_column(nz, q, t)
  END SUBROUTINE compute

END MODULE mo_column_extra
