module openacc

  interface acc_copyin
    module procedure &
      acc_copyin_i_1d, acc_copyin_i_2d, acc_copyin_i_3d, acc_copyin_i_4d, &
      acc_copyin_r_1d, acc_copyin_r_2d, acc_copyin_r_3d, acc_copyin_r_4d, &
      acc_copyin_l_1d, acc_copyin_l_2d, acc_copyin_l_3d, acc_copyin_l_4d
  end interface

contains
integer function acc_get_num_devices( devicetype )
  ! omit kind: integer(acc_device_kind)
  integer ::  devicetype
end function acc_get_num_devices

subroutine acc_set_device_type( devicetype )
  ! omit kind: integer(acc_device_kind)
  integer ::  devicetype
end subroutine acc_set_device_type

function acc_get_device_type()
  ! omit kind: integer(acc_device_kind)
  integer ::  acc_get_device_type
end function acc_get_device_type

subroutine acc_set_device_num( devicenum, devicetype )
  integer ::  devicenum
  ! omit kind: integer(acc_device_kind)
  integer ::  devicetype
end subroutine acc_set_device_num

integer function acc_get_device_num( devicetype )
  ! omit kind: integer(acc_device_kind)
  integer ::  devicetype
end function acc_get_device_num

subroutine acc_init( devicetype )
  ! omit kind: integer(acc_device_kind)
  integer ::  devicetype
end subroutine acc_init

subroutine acc_shutdown( devicetype )
  ! omit kind: integer(acc_device_kind)
  integer ::  devicetype
end subroutine acc_shutdown

logical function acc_async_test( arg )
  ! omit kind: integer(acc_device_kind)
  integer ::  arg
end function acc_async_test

logical function acc_async_test_all( )
end function acc_async_test_all

subroutine acc_wait( arg )
  ! omit kind: integer(acc_device_kind)
  integer ::  arg
end subroutine acc_wait

subroutine acc_wait_async( arg, async )
  ! omit kind: integer(acc_device_kind)
  integer ::  arg, async
end subroutine acc_wait_async

subroutine acc_wait_all( )
end subroutine acc_wait_all

subroutine acc_wait_all_async( async )
  ! omit kind: integer(acc_device_kind)
  integer ::  async
end subroutine acc_wait_all_async

function acc_get_default_async( )
  ! omit kind: integer(acc_device_kind)
  integer ::  acc_get_default_async
end function acc_get_default_async

subroutine acc_set_default_async( async )
  ! omit kind: integer(acc_device_kind)
  integer ::  async
end subroutine acc_set_default_async

logical function acc_on_device( devicetype )
  ! omit kind: integer(acc_device_kind)
  integer ::  devicetype
end function acc_on_device



subroutine acc_copyin_i_1d( a )
  integer, dimension(:) :: a
end subroutine acc_copyin_i_1d

subroutine acc_copyin_i_2d( a )
  integer, dimension(:,:) :: a
end subroutine acc_copyin_i_2d

subroutine acc_copyin_i_3d( a )
  integer, dimension(:,:,:) :: a
end subroutine acc_copyin_i_3d

subroutine acc_copyin_i_4d( a )
  integer, dimension(:,:,:,:) :: a
end subroutine acc_copyin_i_4d

subroutine acc_copyin_r_1d( a )
  real, dimension(:) :: a
end subroutine acc_copyin_r_1d

subroutine acc_copyin_r_2d( a )
  real, dimension(:,:) :: a
end subroutine acc_copyin_r_2d

subroutine acc_copyin_r_3d( a )
  real, dimension(:,:,:) :: a
end subroutine acc_copyin_r_3d

subroutine acc_copyin_r_4d( a )
  real, dimension(:,:,:,:) :: a
end subroutine acc_copyin_r_4d

subroutine acc_copyin_l_1d( a )
  logical, dimension(:) :: a
end subroutine acc_copyin_l_1d

subroutine acc_copyin_l_2d( a )
  logical, dimension(:,:) :: a
end subroutine acc_copyin_l_2d

subroutine acc_copyin_l_3d( a )
  logical, dimension(:,:,:) :: a
end subroutine acc_copyin_l_3d

subroutine acc_copyin_l_4d( a )
  logical, dimension(:,:,:,:) :: a
end subroutine acc_copyin_l_4d

end module openacc
