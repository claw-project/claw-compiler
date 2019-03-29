MODULE mo_column

CONTAINS
 FUNCTION compute_point ( t , nproma , w ) RESULT(q)
  INTEGER , INTENT(IN) :: nproma
  REAL , INTENT(IN) :: t ( 1 : nproma )
  REAL , OPTIONAL , INTENT(IN) :: w ( 1 : nproma )
  REAL :: q ( 1 : nproma )
  REAL :: c
  INTEGER :: proma

!$acc data present(t,w)
!$acc parallel
!$acc loop gang vector
  DO proma = 1 , nproma , 1
   c = 5.345
   q ( proma ) = q ( proma ) + t ( proma ) * c
   IF ( present ( w ) ) THEN
    q ( proma ) = q ( proma ) + w ( proma )
   END IF
  END DO
!$acc end parallel
!$acc end data
 END FUNCTION compute_point

END MODULE mo_column

