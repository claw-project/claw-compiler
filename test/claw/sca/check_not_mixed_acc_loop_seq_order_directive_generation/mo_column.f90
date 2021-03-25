!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test the CLAW abstraction model with one additional dimension.
!
MODULE mod1
  IMPLICIT NONE

  INTEGER :: nvar_vdiff
  INTEGER :: nmatrix
  INTEGER, ALLOCATABLE :: matrix_idx(:)
  INTEGER, ALLOCATABLE :: ibtm_var(:)

  CONTAINS

  SUBROUTINE rhs_bksub( kbdim, itop, klev, aa, bb )
    INTEGER, INTENT(IN) ::  kbdim, itop, klev
    REAL,INTENT(IN)     :: aa(klev,3,nmatrix)
    REAL,INTENT(INOUT)  :: bb(klev,nvar_vdiff)
    INTEGER  :: jvar, im, jk, jkp1

    !$claw define dimension jl(1:kproma) &
    !$claw sca
    DO jvar = 1,nvar_vdiff
      im = matrix_idx(jvar)
      DO jk = ibtm_var(jvar)-1,itop,-1
        jkp1 = jk + 1
        bb(jk,jvar) =  bb(jk  ,jvar) &
          & -bb(jkp1,jvar) &
          & *aa(jk  ,3,im)
      ENDDO
    ENDDO
  END SUBROUTINE rhs_bksub
END MODULE mod1
