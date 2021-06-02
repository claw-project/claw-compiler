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

CONTAINS
 PURE FUNCTION f ( i , j )
  INTEGER , INTENT(IN) :: i
  INTEGER , INTENT(IN) :: j
  REAL :: f

  f = 1.0 * i * j
 END FUNCTION f

 SUBROUTINE call_test ( nrows , ncols )
  USE print_tools

  INTEGER , INTENT(IN) :: nrows
  INTEGER , INTENT(IN) :: ncols
  INTEGER :: r
  INTEGER :: c
  REAL :: array6 ( 1 : nrows , 1 : ncols )
  REAL :: array7 ( 1 : nrows , 1 : ncols )

  DO r = 1 , nrows , 1
   DO c = 1 , ncols , 1
    array6 ( r , c ) = 1.0 * r * c
   END DO
  END DO
  CALL print_rmatrix ("array6" , array6 )
  DO r = 1 , nrows , 1
   DO c = 1 , ncols , 1
    array7 ( r , c ) = f ( r , c ) * 2.0
   END DO
  END DO
  CALL print_rmatrix ("array7" , array7 )
 END SUBROUTINE call_test

END MODULE m

PROGRAM claw_test
  use m
  INTEGER :: nRows = 10
  INTEGER :: nCols = 20
  CALL call_test(nRows, nCols)
END PROGRAM claw_test
