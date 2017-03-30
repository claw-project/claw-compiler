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
      !$claw loop-fusion group(g2) constraint(none)
      DO k=1, kend
        ! loop #2 body here
        sum(k) = sum(k)+i
      END DO

      if (flag) then
        print*,'I did it'
      end if

      !$claw loop-fusion group(g2) constraint(none)
      DO k=1, kend
        ! loop #3 body here
        d = sum(k) + d
      END DO
    ENDDO
    !$claw end loop-hoist
  ENDDO
end program loop_opt
