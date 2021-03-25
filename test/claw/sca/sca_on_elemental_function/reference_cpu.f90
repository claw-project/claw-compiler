MODULE mo_column

CONTAINS
 ELEMENTAL FUNCTION compute_point ( t ) RESULT(q)
  REAL , INTENT(IN) :: t
  REAL :: q
  REAL :: c

  c = 5.345
  q = q + t * c
 END FUNCTION compute_point

END MODULE mo_column

