! Simple program to test the remove directive

PROGRAM remove3
END PROGRAM remove3


MODULE test
  TYPE :: mytype
    character(256), dimension(:), allocatable :: gas_names
    INTEGER :: value
  CONTAINS
    PROCEDURE :: get_ngas
    PROCEDURE :: get_gases
    PROCEDURE :: get_gases1
  END TYPE mytype
CONTAINS



  PURE FUNCTION get_ngas ( this )
   CLASS(mytype) , INTENT(IN) :: this
   INTEGER :: get_ngas
   get_ngas = 10
  END FUNCTION get_ngas

  PURE FUNCTION get_gases1 ( this , arr ) RESULT(gases)
    CLASS(mytype) , INTENT(IN) :: this
    REAL, INTENT(IN) :: arr (10)
    REAL :: gases ( 1 : size(arr, 1) )

  END FUNCTION get_gases1


  PURE FUNCTION get_gases ( this , arr ) RESULT(gases)
    CLASS(mytype) , INTENT(IN) :: this
    REAL, INTENT(IN) :: arr (10)
    REAL :: gases ( 1 : size(arr, 1) )

  END FUNCTION get_gases

END MODULE test
