MODULE mo_column

CONTAINS
 ELEMENTAL FUNCTION compute_point ( t , w ) RESULT(q)
  REAL , INTENT(IN) :: t
  REAL , OPTIONAL , INTENT(IN) :: w
  REAL :: q
  REAL :: c

  c = 5.345
  q = q + t * c
  IF ( present ( w ) ) THEN
   q = q + w
  END IF
 END FUNCTION compute_point

END MODULE mo_column

