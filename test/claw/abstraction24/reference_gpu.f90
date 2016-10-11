MODULE mo_column

CONTAINS
 FUNCTION compute_column ( nz , q , t , nproma ) RESULT(res)
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  REAL :: res ( 1 : nz , 1 : nproma )
  INTEGER , INTENT(IN) :: nproma
  INTEGER :: proma

!$acc data present(t,q,nproma,nz)
!$acc parallel private(k,res,proma,c)
!$acc loop
  DO proma = 1 , nproma , 1
   c = 5.345
!$acc loop seq
   DO k = 2 , nz , 1
    t ( k , proma ) = c * k
    q ( proma , k ) = t ( k - 1 , proma ) + t ( k , proma ) * c
   END DO
   q ( proma , nz ) = q ( proma , nz ) * c
   res = t
  END DO
!$acc end parallel
!$acc end data
 END FUNCTION compute_column

 SUBROUTINE compute_all ( nz , q , val , nproma )
  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: val ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  REAL , TARGET :: z ( 1 : nproma , 1 : nz )
  INTEGER :: k
  REAL , POINTER :: zp ( : , : )
  REAL , TARGET :: res ( 1 : nz , 1 : nproma )
  REAL , POINTER :: res_p ( : , : )
  INTEGER , INTENT(IN) :: nproma
  INTEGER :: iter_nproma

  DO k = 1 , nz , 1
   DO iter_nproma = 1 , nproma , 1
    z ( iter_nproma , k ) = val ( k , iter_nproma ) + q ( iter_nproma , k )
   END DO
  END DO
  zp => z
  res ( 1 : , : ) = compute_column ( nz , q , t = val , nproma = nproma )
  res_p => res
 END SUBROUTINE compute_all

END MODULE mo_column

