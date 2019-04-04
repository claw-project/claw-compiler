PROGRAM serialize
 USE utils_ppser
 USE m_serialize
 INTEGER :: i = 4
 INTEGER :: j = 2
 INTEGER :: k = 6
 REAL :: a ( 1 : 10 )
 REAL :: b ( 1 : 10 )
 REAL :: c ( 1 : 10 )
 REAL :: aa ( 1 : 10 , 1 : 20 )
 REAL :: bb ( 1 : 10 , 1 : 20 )
 REAL :: cc ( 1 : 10 , 1 : 20 )
 REAL :: aaa ( 1 : 10 , 1 : 20 , 1 : 30 )
 REAL :: bbb ( 1 : 10 , 1 : 20 , 1 : 30 )
 REAL :: ccc ( 1 : 10 , 1 : 20 , 1 : 30 )

 CALL fs_create_savepoint ("banana1-input" , ppser_savepoint )
 CALL fs_add_savepoint_metainfo ( ppser_savepoint ,"i" , i )
 CALL fs_add_savepoint_metainfo ( ppser_savepoint ,"k" , k )
 CALL fs_read_field ( ppser_serializer_ref , ppser_savepoint ,&
 "banana1-input_a" , a )
 CALL fs_read_field ( ppser_serializer_ref , ppser_savepoint ,&
 "banana1-input_aa" , aa )
 CALL fs_read_field ( ppser_serializer_ref , ppser_savepoint ,&
 "banana1-input_aaa" , aaa )
 CALL fs_read_field ( ppser_serializer_ref , ppser_savepoint ,&
 "banana1-input_c" , c )
 CALL fs_read_field ( ppser_serializer_ref , ppser_savepoint ,&
 "banana1-input_cc" , cc )
 CALL fs_read_field ( ppser_serializer_ref , ppser_savepoint ,&
 "banana1-input_ccc" , ccc )
 CALL clawserialize ( i , a , aa , aaa , j , b , bb , bbb , k , c , cc , ccc )
 CALL fs_create_savepoint ("banana1-output" , ppser_savepoint )
 CALL fs_add_savepoint_metainfo ( ppser_savepoint ,"i" , i )
 CALL fs_add_savepoint_metainfo ( ppser_savepoint ,"k" , k )
 CALL fs_write_field ( ppser_serializer , ppser_savepoint ,"banana1-output_b"&
  , b )
 CALL fs_write_field ( ppser_serializer , ppser_savepoint ,"banana1-output_bb"&
  , bb )
 CALL fs_write_field ( ppser_serializer , ppser_savepoint ,&
 "banana1-output_bbb" , bbb )
 CALL fs_write_field ( ppser_serializer , ppser_savepoint ,"banana1-output_c"&
  , c )
 CALL fs_write_field ( ppser_serializer , ppser_savepoint ,"banana1-output_cc"&
  , cc )
 CALL fs_write_field ( ppser_serializer , ppser_savepoint ,&
 "banana1-output_ccc" , ccc )

CONTAINS
 SUBROUTINE clawserialize ( i , a , aa , aaa , j , b , bb , bbb , k , c , cc ,&
  ccc )

  INTEGER , INTENT(IN) :: i
  REAL , INTENT(IN) :: a ( : )
  REAL , INTENT(IN) :: aa ( : , : )
  REAL , INTENT(IN) :: aaa ( : , : , : )
  INTEGER , INTENT(OUT) :: j
  REAL , INTENT(OUT) :: b ( : )
  REAL , INTENT(OUT) :: bb ( : , : )
  REAL , INTENT(OUT) :: bbb ( : , : , : )
  INTEGER , INTENT(INOUT) :: k
  REAL , INTENT(INOUT) :: c ( : )
  REAL , INTENT(INOUT) :: cc ( : , : )
  REAL , INTENT(INOUT) :: ccc ( : , : , : )

 END SUBROUTINE clawserialize

END PROGRAM serialize

