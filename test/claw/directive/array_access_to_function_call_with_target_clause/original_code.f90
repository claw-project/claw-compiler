!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the kcache directive
!
module print_tools

implicit none
integer, parameter :: stdout = 6
contains
subroutine print_rmatrix(name, m)
    character(*), intent(in) :: name
    real, dimension(:, :), intent(in) :: m
    integer :: num_rows
    integer :: num_cols
    character(len = 100) :: num_cols_str
    integer :: r, c, use
    num_rows = size(m, 1)
    num_cols = size(m, 2)
    write(num_cols_str, fmt='(I0)') num_cols
    write(unit=stdout, fmt='(A)') name // ':'
    do r = 1, num_rows
        write(unit=stdout, fmt='('// num_cols_str // '(F10.3, A))') (m(r, c), ' ', c=1,num_cols)
    end do
end subroutine print_rmatrix
    end module print_tools

MODULE m
contains
PURE FUNCTION f(i,j)
  INTEGER, INTENT(IN) :: i,j
  REAL :: f
  f = 1.0 * i * j
END FUNCTION

SUBROUTINE call_test(nRows, nCols)
use print_tools
  INTEGER, INTENT(IN) :: nRows, nCols
  INTEGER :: r,c
  REAL, DIMENSION(nRows, nCols) :: array6, array7

  DO r = 1, nRows
    DO c = 1, nCols
      array6(r,c) = 1.0 * r * c
    END DO
  END DO
  call print_rmatrix('array6', array6)

  DO r = 1, nRows
    DO c = 1, nCols
      !$claw call array6=f(r,c) target(gpu)
      array7(r,c) = array6(r,c) * 2.0
    END DO
  END DO
  call print_rmatrix('array7', array7)

  DO r = 1, nRows
    DO c = 1, nCols
      !$claw call array6=f(r,c) target(cpu)
      array7(r,c) = array6(r,c) * 2.0
    END DO
  END DO
  call print_rmatrix('array7', array7)
END SUBROUTINE call_test

END MODULE m

PROGRAM claw_test
  use m
  INTEGER :: nRows = 10
  INTEGER :: nCols = 20
  CALL call_test(nRows, nCols)
END PROGRAM claw_test
