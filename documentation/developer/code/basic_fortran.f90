PROGRAM xcodeml_sample
  IMPLICIT NONE

  INTEGER :: my_integer
  REAL(KIND=8), DIMENSION(10) :: my_double_precision_real_array

  DO my_integer = 1, 10
    my_double_precision_real_array(my_integer) = my_integer ** 2
  END DO
END PROGRAM xcodeml_sample
