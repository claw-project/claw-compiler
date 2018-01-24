!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test code for issue 122 (ECMWF)
!

program loop_opt
  implicit none
  integer :: i,k,j,d
  integer :: iend,jend,kend
  logical :: flag
  integer :: sum(30)

  iend = 20
  jend = 40
  kend = 30
  flag = .FALSE.

  DO j=1,jend
    DO k=1, kend
      ! loop #1 body here
      d = d + 1
      sum(k) = 0
    ENDDO

    !$claw loop-hoist(k) reshape(sum(0))
    DO i=1, iend
      IF( .NOT. flag) THEN
        DO k=1, kend
          ! loop #3 body here
          sum(k) = sum(k)+i
          ! loop # body here
          d = sum(k) + d
        END DO
      ENDIF
    ENDDO
    !$claw end loop-hoist
  ENDDO
end program loop_opt
