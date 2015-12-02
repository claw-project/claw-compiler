module class_Circle
  implicit none
  private
  public :: Circle, circle_area, circle_print

  real :: pi = 3.1415926535897931d0 ! Class-wide private constant

  type Circle
     real :: radius
  end type Circle
contains
  function circle_area(this) result(area)
    type(Circle), intent(in) :: this
    real :: area
    area = pi * this%radius**2
  end function circle_area

  subroutine circle_print(this)
    type(Circle), intent(in) :: this
    real :: area
    area = circle_area(this)  ! Call the circle_area function
    print *, 'Circle: r = ', this%radius, ' area = ', area
  end subroutine circle_print
end module class_Circle

program circle_test
  use class_Circle
  implicit none

  type(Circle) :: c     ! Declare a variable of type Circle.
  c = Circle(1.5)       ! Use the implicit constructor, radius = 1.5.
  call circle_print(c)  ! Call a class subroutine
end program circle_test
