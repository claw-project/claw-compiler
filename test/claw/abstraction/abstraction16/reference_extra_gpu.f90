MODULE mo_column_extra
 USE mo_column , ONLY: ty_column , compute_column

CONTAINS
 SUBROUTINE compute_one ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  TYPE(ty_column) :: column
  INTEGER , INTENT(IN) :: nproma

  CALL column % compute_column ( nz , q , t , nproma = nproma )
 END SUBROUTINE compute_one

END MODULE mo_column_extra

