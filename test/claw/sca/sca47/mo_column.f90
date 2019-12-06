!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test the CLAW abstraction model with one additional dimension.
!

module test_index_mod
   implicit none
contains

   subroutine top_p(vert, p)
      integer, intent(in) :: vert
      real, intent(out):: p(vert)

      !$claw define dimension i(1:IM) &
      !$claw define dimension j(1:JM) &
      !$claw sca data(p) over(i,j,:)

      call set_p(vert, p)

   end

   subroutine set_p(vert, p)
      integer, intent(in) :: vert
      real, intent(out):: p(vert)

      integer :: k

      !$claw sca routine
      do k = 1, vert
         p(k) = 1.0
      end do
   end

end module test_index_mod
