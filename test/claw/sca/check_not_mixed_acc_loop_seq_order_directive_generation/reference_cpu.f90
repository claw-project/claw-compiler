MODULE mod1
 INTEGER :: nvar_vdiff
 INTEGER :: nmatrix
 INTEGER , ALLOCATABLE :: matrix_idx ( : )
 INTEGER , ALLOCATABLE :: ibtm_var ( : )

CONTAINS
 SUBROUTINE rhs_bksub ( kbdim , itop , klev , aa , bb , kproma )
  INTEGER , INTENT(IN) :: kproma

  INTEGER , INTENT(IN) :: kbdim
  INTEGER , INTENT(IN) :: itop
  INTEGER , INTENT(IN) :: klev
  REAL , INTENT(IN) :: aa ( 1 : kproma , 1 : klev , 1 : 3 , 1 : nmatrix )
  REAL , INTENT(INOUT) :: bb ( 1 : kproma , 1 : klev , 1 : nvar_vdiff )
  INTEGER :: jvar
  INTEGER :: im
  INTEGER :: jk
  INTEGER :: jkp1
  INTEGER :: jl

  DO jvar = 1 , nvar_vdiff , 1
   im = matrix_idx ( jvar )
   DO jk = ibtm_var ( jvar ) - 1 , itop , (-1)
    jkp1 = jk + 1
    DO jl = 1 , kproma , 1
     bb ( jl , jk , jvar ) = bb ( jl , jk , jvar ) - bb ( jl , jkp1 , jvar ) *&
      aa ( jl , jk , 3 , im )
    END DO
   END DO
  END DO
 END SUBROUTINE rhs_bksub

END MODULE mod1

