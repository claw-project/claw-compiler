!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column_extra
  USE mo_column, ONLY: ty_column, compute_column
  IMPLICIT NONE

CONTAINS

  SUBROUTINE compute_one(nz, q, t)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    TYPE(ty_column)       :: column

    !$claw parallelize forward
    CALL column%compute_column(nz, q, t)

    ! This code should be automatically parallelized and expanded as do stmt
    q = q + sum(t)

  END SUBROUTINE compute_one

END MODULE mo_column_extra
