MODULE mod1

CONTAINS
 SUBROUTINE sub1 ( )


  PRINT * ,"Call function" ,"sub1"
 END SUBROUTINE sub1

 SUBROUTINE sub2 ( )


  PRINT * ,"Call function" ,"sub2"
  CALL sub1 ( )
 END SUBROUTINE sub2

END MODULE mod1

