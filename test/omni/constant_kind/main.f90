!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test the OMNI Compiler kind specifier
!

PROGRAM constant_kind
  USE mo_column
  REAL(KIND=dp) :: a
  REAL(KIND=sp) :: b
  REAL(KIND=dp) :: c

  a = 10.0_dp
  b = 8.0_sp
  c = a + b + 15.0_sp

END PROGRAM constant_kind
