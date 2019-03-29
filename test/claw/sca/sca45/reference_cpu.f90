MODULE mo_column

CONTAINS
 ELEMENTAL FUNCTION compute_point ( t ) RESULT(q)
  REAL , INTENT(IN) :: t
  REAL :: q
  REAL :: c

  c = 5.345
  q = q + t * c * compute2 ( t )
 END FUNCTION compute_point

 PURE ELEMENTAL FUNCTION compute2 ( t ) RESULT(q)
  REAL , INTENT(IN) :: t
  REAL :: q

  q = t * t
 END FUNCTION compute2

END MODULE mo_column

