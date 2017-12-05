!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

!>
!!  Dummy comment
!!
!! Some comment1
!! Comment that is super long to produce a long line if the fix fails \\\
!! Comment that is super long to produce a long line if the fix fails \\\
!! Comment that is super long to produce a long line if the fix fails \\\
!! Comment that is super long to produce a long line if the fix fails \\\
!! Comment that is super long to produce a long line if the fix fails \\\
!! Comment that is super long to produce a long line if the fix fails \\\
!! Comment that is super long to produce a long line if the fix fails
!!
!!

MODULE mod1

#define DUMMY \
test

  IMPLICIT NONE

  ! More dummy comment with some backslashes \\\\ in the middle
  !
  INTEGER, PARAMETER :: p1 =   6
  INTEGER, PARAMETER :: p2 =  37
  INTEGER, PARAMETER :: p3 =  12
  INTEGER, PARAMETER :: p4 = 307
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(p1,p2) !< comment \
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(p3,p4) !< comment \
  INTEGER, PARAMETER :: wp = dp                        !< selected working precision
  INTEGER, PARAMETER :: vp = wp
  INTEGER, PARAMETER :: vp2 = wp

  INTEGER, PARAMETER :: pi2 =  4
  INTEGER, PARAMETER :: pi4 =  9
  INTEGER, PARAMETER :: pi8 = 14
  !
  INTEGER, PARAMETER :: i2 = SELECTED_INT_KIND(pi2)   ! some int \\
  INTEGER, PARAMETER :: i4 = SELECTED_INT_KIND(pi4)   ! some other int \\\\\
  INTEGER, PARAMETER :: i8 = SELECTED_INT_KIND(pi8)   !< dummy comment here

END MODULE mod1
