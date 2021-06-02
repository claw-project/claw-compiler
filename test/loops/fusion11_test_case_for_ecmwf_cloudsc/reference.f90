PROGRAM ecmwf_case1
 INTEGER :: jl
 INTEGER :: jm

!$acc kernels
!$acc loop
 DO jl = 1 , 10 , 1
  PRINT * ,"1st jl loop body"
!$acc loop
  DO jm = 1 , 10 , 1
   IF ( .TRUE. ) THEN
    PRINT * ,"2nd jm/jl loop body"
   END IF
  END DO
!$acc loop
  DO jm = 1 , 10 , 1
   IF ( .TRUE. ) THEN
    PRINT * ,"4th jm/jl loop body"
   END IF
  END DO
  PRINT * ,"5th jl loop body"
 END DO
!$claw loop-fusion group(g3)
 DO jm = 1 , 10 , 1
  PRINT * ,"3rd jm loop body"
 END DO
!$acc end kernels
END PROGRAM ecmwf_case1

