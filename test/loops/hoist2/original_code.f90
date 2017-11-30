!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
!
! Simple program to test the loop-hoist directive with reshape
! Code is toally dummy and does not perform something useful. Structure is
! important for transformation test
!

PROGRAM claw_test
  CALL claw_hoist2
END PROGRAM claw_test



SUBROUTINE claw_hoist2
  IMPLICIT NONE
  INTEGER :: jt,i,j,ntrac,klev,kproma
  REAL :: var1,var2,var3,var4,var5,time_step_len,zsedtend
  REAL, DIMENSION(10,100) :: array2d_1, array2d_2, array2d_3, array2d_4, &
                             array2d_5
  REAL, DIMENSION(10,100,2) :: array3d_1, array3d_2
  TYPE dummy
    REAL :: value1
    REAL :: value2
  END TYPE dummy
  TYPE(dummy) :: tdum

  zsedtend=1.0
  var3=2.0

  ntrac=2
  klev=10
  kproma=100
  array2d_1(:,:) = 0.0
  array2d_2(:,:) = 0.0
  array2d_3(:,:) = 0.0
  array2d_4(:,:) = 0.0
  array2d_5(:,:) = 0.0

  !$acc parallel loop gang vector collapse(2)
  DO jt=1,ntrac
    !$claw loop-hoist(j,i) reshape(array2d_2(0),array2d_1(1,2)) interchange
    IF (.FALSE.) CYCLE
    var1=tdum%value1
    var2=var1**2.
    IF (jt == 2) THEN
      DO j=1,klev
        DO i=1,kproma
          array2d_2(i,j)=MIN(tdum%value2*2,50.E-6)
        END DO
      END DO
    END IF
    DO j=1,klev
      DO i=1,kproma
        array2d_1(i,j) = 0.0
        IF (array2d_2(i,j) > 0.0) THEN
          var3 = array3d_1(i,j,jt)+array3d_2(i,j,jt)*time_step_len
          var5 = MIN(2.0/9.0, array2d_1(i,j))
          zsedtend = MIN(MAX(0.0, var3 * var5 / array2d_3(i,j)), 2.0)
          array3d_2(i,j,jt) = array3d_2(i,j,jt) - var4
          array2d_1(i,j) = var4 * array2d_4(i,j)
        END IF
      END DO
    END DO
    DO j=2,klev
      DO i=1,kproma
        array3d_2(i,j,jt) = array3d_2(i,j,jt) + (array2d_1(i,j-1) / array2d_5(i,j))
      END DO
    END DO
    !$claw end loop-hoist
    DO j=2,klev
      DO i=1,kproma
        array3d_2(i,j,jt) = array3d_2(i,j,jt) + (5.0 / array2d_5(i,j))
      END DO
    END DO
  END DO
  !$acc end parallel

END SUBROUTINE claw_hoist2
