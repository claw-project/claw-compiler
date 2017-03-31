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

    !$claw loop-hoist(k) reshape(sum(0)) interchange
    DO k=1, kend
      !$claw loop-fusion group(g2) constraint(none)
      DO i=1, iend
        ! loop #2 body here
        sum(i) = sum(i)+i
      END DO

      if (flag) then
        print*,'I did it'
      end if

      !$claw loop-fusion group(g2) constraint(none)
      DO i=1, iend
        ! loop #3 body here
        d = sum(i) + d
      END DO
    ENDDO
    !$claw end loop-hoist
  ENDDO
end program loop_opt
