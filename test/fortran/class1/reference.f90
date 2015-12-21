MODULE class_Circle
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Circle, circle_area, circle_print

  REAL :: pi = 3.1415926535897931d0 ! Class-wide private constant

  TYPE Circle
     real :: radius
  END TYPE Circle
CONTAINS
  FUNCTION circle_area(this) RESULT(area)
    TYPE(Circle), INTENT(IN) :: this
    REAL :: area
    area = pi * this%radius**2
  END FUNCTION circle_area

  SUBROUTINE circle_print(this)
    TYPE(Circle), INTENT(IN) :: this
    REAL :: area
    area = circle_area(this)  ! Call the circle_area function
    PRINT *, 'Circle: r = ', this%radius, ' area = ', area
  END SUBROUTINE circle_print
END MODULE class_Circle

PROGRAM circle_test
  USE class_Circle
  IMPLICIT NONE

  TYPE(Circle) :: c     ! Declare a variable of type Circle.
  c = Circle(1.5)       ! Use the implicit constructor, radius = 1.5.
  CALL circle_print(c)  ! Call a class subroutine
END PROGRAM circle_test
