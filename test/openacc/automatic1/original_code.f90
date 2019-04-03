PROGRAM automatic_openacc
REAL,DIMENSION(10) :: g1
REAL,DIMENSION(10) :: ga, gb

    ga(:)=2
    gb(:)=0
    g1(:)=1
    CALL my_subroutine(ga, gb)

CONTAINS

SUBROUTINE my_subroutine(a, b)
    REAL, DIMENSION(10), INTENT(IN) :: a
    REAL, DIMENSION(10), INTENT(OUT) :: b
    
    REAL, DIMENSION(10) :: loc1
    INTEGER :: i

    !$CLAW autoport
    DO i=1,10
        loc1(i) = a(i)+10+g1(i)
        b(i) = loc1(i)/2
    END DO
    !$CLAW END autoport


END SUBROUTINE my_subroutine

END PROGRAM
