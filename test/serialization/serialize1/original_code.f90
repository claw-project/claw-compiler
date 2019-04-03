!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the serialize directive
!
PROGRAM serialize
  INTEGER :: i = 4
  INTEGER :: j = 2
  INTEGER :: k = 6
  REAL, DIMENSION(10) :: a, b, c
  REAL, DIMENSION(10,20) :: aa, bb, cc
  REAL, DIMENSION(10,20,30) :: aaa, bbb, ccc

  !$claw serialize banana1 write
  CALL clawserialize(i, a, aa, aaa,j ,b, bb, bbb,k ,c, cc, ccc)

  CONTAINS

    SUBROUTINE clawserialize(i, a, aa, aaa, j, b, bb, bbb, k, c, cc, ccc)
      INTEGER, INTENT(IN)     :: i
      REAL, INTENT(IN)        :: a(:), aa(:,:), aaa(:,:,:)
      INTEGER, INTENT(OUT)    :: j
      REAL, INTENT(OUT)       :: b(:), bb(:,:), bbb(:,:,:)
      INTEGER, INTENT(INOUT)  :: k
      REAL, INTENT(INOUT)     :: c(:), cc(:,:), ccc(:,:,:)


    END SUBROUTINE clawserialize

END PROGRAM serialize
