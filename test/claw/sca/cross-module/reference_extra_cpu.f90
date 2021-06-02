MODULE mo_column_extra

CONTAINS
 SUBROUTINE compute ( nz , q , t , nproma )
  USE mo_column
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )

  CALL compute_column ( nz , q , t , nproma = nproma )
 END SUBROUTINE compute

END MODULE mo_column_extra

