PROGRAM constant_kind
 USE mo_column
 REAL ( KIND= selected_real_kind ( 13 ) ) :: a
 REAL ( KIND= selected_real_kind ( 6 ) ) :: b
 REAL ( KIND= selected_real_kind ( 13 ) ) :: c

 a = 10.0_dp
 b = 8.0_sp
 c = a + b + 15.0_sp
END PROGRAM constant_kind

