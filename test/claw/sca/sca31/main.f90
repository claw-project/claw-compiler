!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test the CLAW abstraction model with one additional dimension.
!

PROGRAM test_abstraction31
  USE mo_column, ONLY: compute, t1
  REAL, DIMENSION(20,60) :: q  ! Fields as declared in the whole model
  REAL, DIMENSION(20)    :: z
  TYPE(t1) :: ty
  INTEGER :: nproma, nz           ! Size of array fields
  INTEGER :: p                    ! Loop index

  nproma = 20
  nz = 60
  z = 10.0

  DO p = 1, nproma
    q(p,1) = 0.0
  END DO

  ALLOCATE(ty%y(nproma,nz))

  !$claw sca forward create update
  DO p = 1, nproma
    CALL compute(nz, q(p,:), ty%y(p,:), z(p))
  END DO
  
  PRINT*,SUM(q)
  PRINT*,SUM(ty%y)

  DEALLOCATE(ty%y)
END PROGRAM test_abstraction31
