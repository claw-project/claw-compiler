PROGRAM testverbatim
!$claw verbatim IF (.FALSE.) THEN
PRINT*,'These lines'
PRINT*,'are not printed'
PRINT*,'if the the CLAW compiler has processed'
PRINT*,'the file.'
!$claw verbatim END IF
END PROGRAM testverbatim
