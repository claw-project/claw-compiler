!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the kcache directive
!

PROGRAM claw_test
  INTEGER :: istart = 1
  INTEGER :: iend = 10
  INTEGER :: jstart = 1
  INTEGER :: jend = 20
  CALL kcache(istart,iend,jstart,jend)
END PROGRAM claw_test

SUBROUTINE kcache(istart,iend,jstart,jend)
  INTEGER, INTENT(IN) :: istart, iend, jstart, jend
  INTEGER :: i,j
  REAL(KIND=8), DIMENSION(istart:iend,jstart:jend) :: array6, array7, array8, &
                                                      array9, data1, data2

  data1(:,:) = 2.0
  data2(:,:) = 3.0

  array6(:, :) = 0.0
  array7(:, :) = 0.0
  array8(:, :) = 0.0
  array9(:, :) = 0.0

  DO i = istart, iend
    array6(i,1) = 1.0
    array7(i,1) = 2.0
    array8(i,1) = 3.0
    array9(i,1) = 4.0
  END DO

  DO i = istart, iend
    DO j = jstart+1, jend
      !$claw kcache data(data1, data2)
      !$claw kcache data(array6, array7, array8, array9) offset(0,-1) init
      array6(i,j) = array6(i,j-1) * 2.0 + data1(i,j)
      array7(i,j) = array7(i,j-1) * 2.0 + array6(i,j-1) + data1(i,j)
      array8(i,j) = array8(i,j-1) * 2.0 + array6(i,j-1) + array7(i,j-1) +  &
                    data2(i,j)
      array9(i,j) = array9(i,j-1) * 2.0 + array6(i,j-1) + array8(i,j-1) +  &
                    data2(i,j)
    END DO
  END DO
  PRINT*, SUM(array6)
  PRINT*, SUM(array7)
  PRINT*, SUM(array8)
  PRINT*, SUM(array9)
END SUBROUTINE kcache
