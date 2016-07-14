MODULE mo_column_extra

CONTAINS
 SUBROUTINE compute_one ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER , INTENT(IN) :: nproma

  CALL compute_two ( nz , q , t , nproma = nproma )
 END SUBROUTINE compute_one

 SUBROUTINE compute_two ( nz , q , t , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER , INTENT(IN) :: nproma

  CALL compute_three ( nz , q , t , nproma = nproma )
 END SUBROUTINE compute_two

 SUBROUTINE compute_three ( nz , q , t , nproma )
  USE mo_column
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER , INTENT(IN) :: nproma

  CALL compute_solver ( nz , q , t , nproma = nproma )
 END SUBROUTINE compute_three

END MODULE mo_column_extra

