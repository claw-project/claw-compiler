MODULE class_circle
 REAL , PRIVATE :: pi = 3.1415926535897931d0
 TYPE , PUBLIC :: circle
  REAL :: radius
 END TYPE circle

CONTAINS
 FUNCTION circle_area ( this ) RESULT(area)
  REAL :: area
  TYPE(circle) , INTENT(IN) :: this

  area = pi * this % radius ** ( 2 )
 END FUNCTION circle_area

 SUBROUTINE circle_print ( this )
  TYPE(circle) , INTENT(IN) :: this
  REAL :: area

  area = circle_area ( this )
  PRINT * ,"Circle: r = " , this % radius ," area = " , area
 END SUBROUTINE circle_print

END MODULE class_circle

PROGRAM circle_test
 USE class_circle
 TYPE(circle) :: c

 c = circle ( 1.5 )
 CALL circle_print ( c )
END PROGRAM circle_test

