!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
!
! @author clementval
! @brief  This file is used as a signatutre only module to generate .xmod file.
!         These files are needed during a full parse with the OMNI Compiler.
!         This file is partially automatically generated.

module openacc

  ! Automatically generated signatures for acc_copyin
  interface acc_copyin
    module procedure &
      acc_copyin_i_1d_p1, &
      acc_copyin_i_2d_p1, &
      acc_copyin_i_3d_p1, &
      acc_copyin_i_4d_p1, &
      acc_copyin_r_1d_p1, &
      acc_copyin_r_2d_p1, &
      acc_copyin_r_3d_p1, &
      acc_copyin_r_4d_p1, &
      acc_copyin_l_1d_p1, &
      acc_copyin_l_2d_p1, &
      acc_copyin_l_3d_p1, &
      acc_copyin_l_4d_p1, &
      acc_copyin_c_1d_p1, &
      acc_copyin_c_2d_p1, &
      acc_copyin_c_3d_p1, &
      acc_copyin_c_4d_p1, &
      acc_copyin_i_l_p2, &
      acc_copyin_r_l_p2, &
      acc_copyin_l_l_p2, &
      acc_copyin_c_l_p2
  end interface

  ! Automatically generated signatures for acc_copyin_async
  interface acc_copyin_async
    module procedure &
      acc_copyin_async_i_1d_p2, &
      acc_copyin_async_i_2d_p2, &
      acc_copyin_async_i_3d_p2, &
      acc_copyin_async_i_4d_p2, &
      acc_copyin_async_r_1d_p2, &
      acc_copyin_async_r_2d_p2, &
      acc_copyin_async_r_3d_p2, &
      acc_copyin_async_r_4d_p2, &
      acc_copyin_async_l_1d_p2, &
      acc_copyin_async_l_2d_p2, &
      acc_copyin_async_l_3d_p2, &
      acc_copyin_async_l_4d_p2, &
      acc_copyin_async_c_1d_p2, &
      acc_copyin_async_c_2d_p2, &
      acc_copyin_async_c_3d_p2, &
      acc_copyin_async_c_4d_p2, &
      acc_copyin_async_i_l_p3, &
      acc_copyin_async_r_l_p3, &
      acc_copyin_async_l_l_p3, &
      acc_copyin_async_c_l_p3
  end interface

  ! Automatically generated signatures for acc_create
  interface acc_create
    module procedure &
      acc_create_i_1d_p1, &
      acc_create_i_2d_p1, &
      acc_create_i_3d_p1, &
      acc_create_i_4d_p1, &
      acc_create_r_1d_p1, &
      acc_create_r_2d_p1, &
      acc_create_r_3d_p1, &
      acc_create_r_4d_p1, &
      acc_create_l_1d_p1, &
      acc_create_l_2d_p1, &
      acc_create_l_3d_p1, &
      acc_create_l_4d_p1, &
      acc_create_c_1d_p1, &
      acc_create_c_2d_p1, &
      acc_create_c_3d_p1, &
      acc_create_c_4d_p1, &
      acc_create_i_l_p2, &
      acc_create_r_l_p2, &
      acc_create_l_l_p2, &
      acc_create_c_l_p2
  end interface

  ! Automatically generated signatures for acc_create_async
  interface acc_create_async
    module procedure &
      acc_create_async_i_1d_p2, &
      acc_create_async_i_2d_p2, &
      acc_create_async_i_3d_p2, &
      acc_create_async_i_4d_p2, &
      acc_create_async_r_1d_p2, &
      acc_create_async_r_2d_p2, &
      acc_create_async_r_3d_p2, &
      acc_create_async_r_4d_p2, &
      acc_create_async_l_1d_p2, &
      acc_create_async_l_2d_p2, &
      acc_create_async_l_3d_p2, &
      acc_create_async_l_4d_p2, &
      acc_create_async_c_1d_p2, &
      acc_create_async_c_2d_p2, &
      acc_create_async_c_3d_p2, &
      acc_create_async_c_4d_p2, &
      acc_create_async_i_l_p3, &
      acc_create_async_r_l_p3, &
      acc_create_async_l_l_p3, &
      acc_create_async_c_l_p3
  end interface

  ! Automatically generated signatures for acc_copyout
  interface acc_copyout
    module procedure &
      acc_copyout_i_1d_p1, &
      acc_copyout_i_2d_p1, &
      acc_copyout_i_3d_p1, &
      acc_copyout_i_4d_p1, &
      acc_copyout_r_1d_p1, &
      acc_copyout_r_2d_p1, &
      acc_copyout_r_3d_p1, &
      acc_copyout_r_4d_p1, &
      acc_copyout_l_1d_p1, &
      acc_copyout_l_2d_p1, &
      acc_copyout_l_3d_p1, &
      acc_copyout_l_4d_p1, &
      acc_copyout_c_1d_p1, &
      acc_copyout_c_2d_p1, &
      acc_copyout_c_3d_p1, &
      acc_copyout_c_4d_p1, &
      acc_copyout_i_l_p2, &
      acc_copyout_r_l_p2, &
      acc_copyout_l_l_p2, &
      acc_copyout_c_l_p2
  end interface

  ! Automatically generated signatures for acc_copyout_async
  interface acc_copyout_async
    module procedure &
      acc_copyout_async_i_1d_p2, &
      acc_copyout_async_i_2d_p2, &
      acc_copyout_async_i_3d_p2, &
      acc_copyout_async_i_4d_p2, &
      acc_copyout_async_r_1d_p2, &
      acc_copyout_async_r_2d_p2, &
      acc_copyout_async_r_3d_p2, &
      acc_copyout_async_r_4d_p2, &
      acc_copyout_async_l_1d_p2, &
      acc_copyout_async_l_2d_p2, &
      acc_copyout_async_l_3d_p2, &
      acc_copyout_async_l_4d_p2, &
      acc_copyout_async_c_1d_p2, &
      acc_copyout_async_c_2d_p2, &
      acc_copyout_async_c_3d_p2, &
      acc_copyout_async_c_4d_p2, &
      acc_copyout_async_i_l_p3, &
      acc_copyout_async_r_l_p3, &
      acc_copyout_async_l_l_p3, &
      acc_copyout_async_c_l_p3
  end interface

  ! Automatically generated signatures for acc_copyout_finalize
  interface acc_copyout_finalize
    module procedure &
      acc_copyout_finalize_i_1d_p1, &
      acc_copyout_finalize_i_2d_p1, &
      acc_copyout_finalize_i_3d_p1, &
      acc_copyout_finalize_i_4d_p1, &
      acc_copyout_finalize_r_1d_p1, &
      acc_copyout_finalize_r_2d_p1, &
      acc_copyout_finalize_r_3d_p1, &
      acc_copyout_finalize_r_4d_p1, &
      acc_copyout_finalize_l_1d_p1, &
      acc_copyout_finalize_l_2d_p1, &
      acc_copyout_finalize_l_3d_p1, &
      acc_copyout_finalize_l_4d_p1, &
      acc_copyout_finalize_c_1d_p1, &
      acc_copyout_finalize_c_2d_p1, &
      acc_copyout_finalize_c_3d_p1, &
      acc_copyout_finalize_c_4d_p1, &
      acc_copyout_finalize_i_l_p2, &
      acc_copyout_finalize_r_l_p2, &
      acc_copyout_finalize_l_l_p2, &
      acc_copyout_finalize_c_l_p2
  end interface

  ! Automatically generated signatures for acc_copyout_finalize_async
  interface acc_copyout_finalize_async
    module procedure &
      acc_copyout_finalize_async_i_1d_p2, &
      acc_copyout_finalize_async_i_2d_p2, &
      acc_copyout_finalize_async_i_3d_p2, &
      acc_copyout_finalize_async_i_4d_p2, &
      acc_copyout_finalize_async_r_1d_p2, &
      acc_copyout_finalize_async_r_2d_p2, &
      acc_copyout_finalize_async_r_3d_p2, &
      acc_copyout_finalize_async_r_4d_p2, &
      acc_copyout_finalize_async_l_1d_p2, &
      acc_copyout_finalize_async_l_2d_p2, &
      acc_copyout_finalize_async_l_3d_p2, &
      acc_copyout_finalize_async_l_4d_p2, &
      acc_copyout_finalize_async_c_1d_p2, &
      acc_copyout_finalize_async_c_2d_p2, &
      acc_copyout_finalize_async_c_3d_p2, &
      acc_copyout_finalize_async_c_4d_p2, &
      acc_copyout_finalize_async_i_l_p3, &
      acc_copyout_finalize_async_r_l_p3, &
      acc_copyout_finalize_async_l_l_p3, &
      acc_copyout_finalize_async_c_l_p3
  end interface

  ! Automatically generated signatures for acc_delete
  interface acc_delete
    module procedure &
      acc_delete_i_1d_p1, &
      acc_delete_i_2d_p1, &
      acc_delete_i_3d_p1, &
      acc_delete_i_4d_p1, &
      acc_delete_r_1d_p1, &
      acc_delete_r_2d_p1, &
      acc_delete_r_3d_p1, &
      acc_delete_r_4d_p1, &
      acc_delete_l_1d_p1, &
      acc_delete_l_2d_p1, &
      acc_delete_l_3d_p1, &
      acc_delete_l_4d_p1, &
      acc_delete_c_1d_p1, &
      acc_delete_c_2d_p1, &
      acc_delete_c_3d_p1, &
      acc_delete_c_4d_p1, &
      acc_delete_i_l_p2, &
      acc_delete_r_l_p2, &
      acc_delete_l_l_p2, &
      acc_delete_c_l_p2
  end interface

  ! Automatically generated signatures for acc_delete_async
  interface acc_delete_async
    module procedure &
      acc_delete_async_i_1d_p2, &
      acc_delete_async_i_2d_p2, &
      acc_delete_async_i_3d_p2, &
      acc_delete_async_i_4d_p2, &
      acc_delete_async_r_1d_p2, &
      acc_delete_async_r_2d_p2, &
      acc_delete_async_r_3d_p2, &
      acc_delete_async_r_4d_p2, &
      acc_delete_async_l_1d_p2, &
      acc_delete_async_l_2d_p2, &
      acc_delete_async_l_3d_p2, &
      acc_delete_async_l_4d_p2, &
      acc_delete_async_c_1d_p2, &
      acc_delete_async_c_2d_p2, &
      acc_delete_async_c_3d_p2, &
      acc_delete_async_c_4d_p2, &
      acc_delete_async_i_l_p3, &
      acc_delete_async_r_l_p3, &
      acc_delete_async_l_l_p3, &
      acc_delete_async_c_l_p3
  end interface

  ! Automatically generated signatures for acc_delete_finalize
  interface acc_delete_finalize
    module procedure &
      acc_delete_finalize_i_1d_p1, &
      acc_delete_finalize_i_2d_p1, &
      acc_delete_finalize_i_3d_p1, &
      acc_delete_finalize_i_4d_p1, &
      acc_delete_finalize_r_1d_p1, &
      acc_delete_finalize_r_2d_p1, &
      acc_delete_finalize_r_3d_p1, &
      acc_delete_finalize_r_4d_p1, &
      acc_delete_finalize_l_1d_p1, &
      acc_delete_finalize_l_2d_p1, &
      acc_delete_finalize_l_3d_p1, &
      acc_delete_finalize_l_4d_p1, &
      acc_delete_finalize_c_1d_p1, &
      acc_delete_finalize_c_2d_p1, &
      acc_delete_finalize_c_3d_p1, &
      acc_delete_finalize_c_4d_p1, &
      acc_delete_finalize_i_l_p2, &
      acc_delete_finalize_r_l_p2, &
      acc_delete_finalize_l_l_p2, &
      acc_delete_finalize_c_l_p2
  end interface

  ! Automatically generated signatures for acc_delete_finalize_async
  interface acc_delete_finalize_async
    module procedure &
      acc_delete_finalize_async_i_1d_p2, &
      acc_delete_finalize_async_i_2d_p2, &
      acc_delete_finalize_async_i_3d_p2, &
      acc_delete_finalize_async_i_4d_p2, &
      acc_delete_finalize_async_r_1d_p2, &
      acc_delete_finalize_async_r_2d_p2, &
      acc_delete_finalize_async_r_3d_p2, &
      acc_delete_finalize_async_r_4d_p2, &
      acc_delete_finalize_async_l_1d_p2, &
      acc_delete_finalize_async_l_2d_p2, &
      acc_delete_finalize_async_l_3d_p2, &
      acc_delete_finalize_async_l_4d_p2, &
      acc_delete_finalize_async_c_1d_p2, &
      acc_delete_finalize_async_c_2d_p2, &
      acc_delete_finalize_async_c_3d_p2, &
      acc_delete_finalize_async_c_4d_p2, &
      acc_delete_finalize_async_i_l_p3, &
      acc_delete_finalize_async_r_l_p3, &
      acc_delete_finalize_async_l_l_p3, &
      acc_delete_finalize_async_c_l_p3
  end interface

  ! Automatically generated signatures for acc_update_device
  interface acc_update_device
    module procedure &
      acc_update_device_i_1d_p1, &
      acc_update_device_i_2d_p1, &
      acc_update_device_i_3d_p1, &
      acc_update_device_i_4d_p1, &
      acc_update_device_r_1d_p1, &
      acc_update_device_r_2d_p1, &
      acc_update_device_r_3d_p1, &
      acc_update_device_r_4d_p1, &
      acc_update_device_l_1d_p1, &
      acc_update_device_l_2d_p1, &
      acc_update_device_l_3d_p1, &
      acc_update_device_l_4d_p1, &
      acc_update_device_c_1d_p1, &
      acc_update_device_c_2d_p1, &
      acc_update_device_c_3d_p1, &
      acc_update_device_c_4d_p1, &
      acc_update_device_i_l_p2, &
      acc_update_device_r_l_p2, &
      acc_update_device_l_l_p2, &
      acc_update_device_c_l_p2
  end interface

  ! Automatically generated signatures for acc_update_device_async
  interface acc_update_device_async
    module procedure &
      acc_update_device_async_i_1d_p2, &
      acc_update_device_async_i_2d_p2, &
      acc_update_device_async_i_3d_p2, &
      acc_update_device_async_i_4d_p2, &
      acc_update_device_async_r_1d_p2, &
      acc_update_device_async_r_2d_p2, &
      acc_update_device_async_r_3d_p2, &
      acc_update_device_async_r_4d_p2, &
      acc_update_device_async_l_1d_p2, &
      acc_update_device_async_l_2d_p2, &
      acc_update_device_async_l_3d_p2, &
      acc_update_device_async_l_4d_p2, &
      acc_update_device_async_c_1d_p2, &
      acc_update_device_async_c_2d_p2, &
      acc_update_device_async_c_3d_p2, &
      acc_update_device_async_c_4d_p2, &
      acc_update_device_async_i_l_p3, &
      acc_update_device_async_r_l_p3, &
      acc_update_device_async_l_l_p3, &
      acc_update_device_async_c_l_p3
  end interface

  ! Automatically generated signatures for acc_update_self
  interface acc_update_self
    module procedure &
      acc_update_self_i_1d_p1, &
      acc_update_self_i_2d_p1, &
      acc_update_self_i_3d_p1, &
      acc_update_self_i_4d_p1, &
      acc_update_self_r_1d_p1, &
      acc_update_self_r_2d_p1, &
      acc_update_self_r_3d_p1, &
      acc_update_self_r_4d_p1, &
      acc_update_self_l_1d_p1, &
      acc_update_self_l_2d_p1, &
      acc_update_self_l_3d_p1, &
      acc_update_self_l_4d_p1, &
      acc_update_self_c_1d_p1, &
      acc_update_self_c_2d_p1, &
      acc_update_self_c_3d_p1, &
      acc_update_self_c_4d_p1, &
      acc_update_self_i_l_p2, &
      acc_update_self_r_l_p2, &
      acc_update_self_l_l_p2, &
      acc_update_self_c_l_p2
  end interface

  ! Automatically generated signatures for acc_update_self_async
  interface acc_update_self_async
    module procedure &
      acc_update_self_async_i_1d_p2, &
      acc_update_self_async_i_2d_p2, &
      acc_update_self_async_i_3d_p2, &
      acc_update_self_async_i_4d_p2, &
      acc_update_self_async_r_1d_p2, &
      acc_update_self_async_r_2d_p2, &
      acc_update_self_async_r_3d_p2, &
      acc_update_self_async_r_4d_p2, &
      acc_update_self_async_l_1d_p2, &
      acc_update_self_async_l_2d_p2, &
      acc_update_self_async_l_3d_p2, &
      acc_update_self_async_l_4d_p2, &
      acc_update_self_async_c_1d_p2, &
      acc_update_self_async_c_2d_p2, &
      acc_update_self_async_c_3d_p2, &
      acc_update_self_async_c_4d_p2, &
      acc_update_self_async_i_l_p3, &
      acc_update_self_async_r_l_p3, &
      acc_update_self_async_l_l_p3, &
      acc_update_self_async_c_l_p3
  end interface

  ! Automatically generated signatures for acc_is_present
  interface acc_is_present
    module procedure &
      acc_is_present_i_1d_p1, &
      acc_is_present_i_2d_p1, &
      acc_is_present_i_3d_p1, &
      acc_is_present_i_4d_p1, &
      acc_is_present_r_1d_p1, &
      acc_is_present_r_2d_p1, &
      acc_is_present_r_3d_p1, &
      acc_is_present_r_4d_p1, &
      acc_is_present_l_1d_p1, &
      acc_is_present_l_2d_p1, &
      acc_is_present_l_3d_p1, &
      acc_is_present_l_4d_p1, &
      acc_is_present_c_1d_p1, &
      acc_is_present_c_2d_p1, &
      acc_is_present_c_3d_p1, &
      acc_is_present_c_4d_p1, &
      acc_is_present_i_l_p2, &
      acc_is_present_r_l_p2, &
      acc_is_present_l_l_p2, &
      acc_is_present_c_l_p2
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

! ------------------------------
! automatically generated part
! ------------------------------
! Automatically generated signatures for acc_copyin
subroutine acc_copyin_i_1d_p1( a )
  integer, dimension(:) :: a
end subroutine acc_copyin_i_1d_p1

subroutine acc_copyin_i_2d_p1( a )
  integer, dimension(:,:) :: a
end subroutine acc_copyin_i_2d_p1

subroutine acc_copyin_i_3d_p1( a )
  integer, dimension(:,:,:) :: a
end subroutine acc_copyin_i_3d_p1

subroutine acc_copyin_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a
end subroutine acc_copyin_i_4d_p1

subroutine acc_copyin_r_1d_p1( a )
  real, dimension(:) :: a
end subroutine acc_copyin_r_1d_p1

subroutine acc_copyin_r_2d_p1( a )
  real, dimension(:,:) :: a
end subroutine acc_copyin_r_2d_p1

subroutine acc_copyin_r_3d_p1( a )
  real, dimension(:,:,:) :: a
end subroutine acc_copyin_r_3d_p1

subroutine acc_copyin_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a
end subroutine acc_copyin_r_4d_p1

subroutine acc_copyin_l_1d_p1( a )
  logical, dimension(:) :: a
end subroutine acc_copyin_l_1d_p1

subroutine acc_copyin_l_2d_p1( a )
  logical, dimension(:,:) :: a
end subroutine acc_copyin_l_2d_p1

subroutine acc_copyin_l_3d_p1( a )
  logical, dimension(:,:,:) :: a
end subroutine acc_copyin_l_3d_p1

subroutine acc_copyin_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a
end subroutine acc_copyin_l_4d_p1

subroutine acc_copyin_c_1d_p1( a )
  character, dimension(:) :: a
end subroutine acc_copyin_c_1d_p1

subroutine acc_copyin_c_2d_p1( a )
  character, dimension(:,:) :: a
end subroutine acc_copyin_c_2d_p1

subroutine acc_copyin_c_3d_p1( a )
  character, dimension(:,:,:) :: a
end subroutine acc_copyin_c_3d_p1

subroutine acc_copyin_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a
end subroutine acc_copyin_c_4d_p1

! Automatically generated signatures for acc_copyin
subroutine acc_copyin_i_l_p2( a , len )
  integer :: a
  integer :: len
end subroutine acc_copyin_i_l_p2

subroutine acc_copyin_r_l_p2( a , len )
  real :: a
  integer :: len
end subroutine acc_copyin_r_l_p2

subroutine acc_copyin_l_l_p2( a , len )
  logical :: a
  integer :: len
end subroutine acc_copyin_l_l_p2

subroutine acc_copyin_c_l_p2( a , len )
  character :: a
  integer :: len
end subroutine acc_copyin_c_l_p2

! Automatically generated signatures for acc_copyin_async
subroutine acc_copyin_async_i_1d_p2( a, async )
  integer, dimension(:) :: a
  integer :: async
end subroutine acc_copyin_async_i_1d_p2

subroutine acc_copyin_async_i_2d_p2( a, async )
  integer, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyin_async_i_2d_p2

subroutine acc_copyin_async_i_3d_p2( a, async )
  integer, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyin_async_i_3d_p2

subroutine acc_copyin_async_i_4d_p2( a, async )
  integer, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyin_async_i_4d_p2

subroutine acc_copyin_async_r_1d_p2( a, async )
  real, dimension(:) :: a
  integer :: async
end subroutine acc_copyin_async_r_1d_p2

subroutine acc_copyin_async_r_2d_p2( a, async )
  real, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyin_async_r_2d_p2

subroutine acc_copyin_async_r_3d_p2( a, async )
  real, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyin_async_r_3d_p2

subroutine acc_copyin_async_r_4d_p2( a, async )
  real, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyin_async_r_4d_p2

subroutine acc_copyin_async_l_1d_p2( a, async )
  logical, dimension(:) :: a
  integer :: async
end subroutine acc_copyin_async_l_1d_p2

subroutine acc_copyin_async_l_2d_p2( a, async )
  logical, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyin_async_l_2d_p2

subroutine acc_copyin_async_l_3d_p2( a, async )
  logical, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyin_async_l_3d_p2

subroutine acc_copyin_async_l_4d_p2( a, async )
  logical, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyin_async_l_4d_p2

subroutine acc_copyin_async_c_1d_p2( a, async )
  character, dimension(:) :: a
  integer :: async
end subroutine acc_copyin_async_c_1d_p2

subroutine acc_copyin_async_c_2d_p2( a, async )
  character, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyin_async_c_2d_p2

subroutine acc_copyin_async_c_3d_p2( a, async )
  character, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyin_async_c_3d_p2

subroutine acc_copyin_async_c_4d_p2( a, async )
  character, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyin_async_c_4d_p2

! Automatically generated signatures for acc_copyin_async
subroutine acc_copyin_async_i_l_p3( a, len, async )
  integer :: a
  integer :: len
  integer :: async
end subroutine acc_copyin_async_i_l_p3

subroutine acc_copyin_async_r_l_p3( a, len, async )
  real :: a
  integer :: len
  integer :: async
end subroutine acc_copyin_async_r_l_p3

subroutine acc_copyin_async_l_l_p3( a, len, async )
  logical :: a
  integer :: len
  integer :: async
end subroutine acc_copyin_async_l_l_p3

subroutine acc_copyin_async_c_l_p3( a, len, async )
  character :: a
  integer :: len
  integer :: async
end subroutine acc_copyin_async_c_l_p3

! Automatically generated signatures for acc_create
subroutine acc_create_i_1d_p1( a )
  integer, dimension(:) :: a
end subroutine acc_create_i_1d_p1

subroutine acc_create_i_2d_p1( a )
  integer, dimension(:,:) :: a
end subroutine acc_create_i_2d_p1

subroutine acc_create_i_3d_p1( a )
  integer, dimension(:,:,:) :: a
end subroutine acc_create_i_3d_p1

subroutine acc_create_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a
end subroutine acc_create_i_4d_p1

subroutine acc_create_r_1d_p1( a )
  real, dimension(:) :: a
end subroutine acc_create_r_1d_p1

subroutine acc_create_r_2d_p1( a )
  real, dimension(:,:) :: a
end subroutine acc_create_r_2d_p1

subroutine acc_create_r_3d_p1( a )
  real, dimension(:,:,:) :: a
end subroutine acc_create_r_3d_p1

subroutine acc_create_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a
end subroutine acc_create_r_4d_p1

subroutine acc_create_l_1d_p1( a )
  logical, dimension(:) :: a
end subroutine acc_create_l_1d_p1

subroutine acc_create_l_2d_p1( a )
  logical, dimension(:,:) :: a
end subroutine acc_create_l_2d_p1

subroutine acc_create_l_3d_p1( a )
  logical, dimension(:,:,:) :: a
end subroutine acc_create_l_3d_p1

subroutine acc_create_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a
end subroutine acc_create_l_4d_p1

subroutine acc_create_c_1d_p1( a )
  character, dimension(:) :: a
end subroutine acc_create_c_1d_p1

subroutine acc_create_c_2d_p1( a )
  character, dimension(:,:) :: a
end subroutine acc_create_c_2d_p1

subroutine acc_create_c_3d_p1( a )
  character, dimension(:,:,:) :: a
end subroutine acc_create_c_3d_p1

subroutine acc_create_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a
end subroutine acc_create_c_4d_p1

! Automatically generated signatures for acc_create
subroutine acc_create_i_l_p2( a, len )
  integer :: a
  integer :: len
end subroutine acc_create_i_l_p2

subroutine acc_create_r_l_p2( a, len )
  real :: a
  integer :: len
end subroutine acc_create_r_l_p2

subroutine acc_create_l_l_p2( a, len )
  logical :: a
  integer :: len
end subroutine acc_create_l_l_p2

subroutine acc_create_c_l_p2( a, len )
  character :: a
  integer :: len
end subroutine acc_create_c_l_p2

! Automatically generated signatures for acc_create_async
subroutine acc_create_async_i_1d_p2( a, async )
  integer, dimension(:) :: a
  integer :: async
end subroutine acc_create_async_i_1d_p2

subroutine acc_create_async_i_2d_p2( a, async )
  integer, dimension(:,:) :: a
  integer :: async
end subroutine acc_create_async_i_2d_p2

subroutine acc_create_async_i_3d_p2( a, async )
  integer, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_create_async_i_3d_p2

subroutine acc_create_async_i_4d_p2( a, async )
  integer, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_create_async_i_4d_p2

subroutine acc_create_async_r_1d_p2( a, async )
  real, dimension(:) :: a
  integer :: async
end subroutine acc_create_async_r_1d_p2

subroutine acc_create_async_r_2d_p2( a, async )
  real, dimension(:,:) :: a
  integer :: async
end subroutine acc_create_async_r_2d_p2

subroutine acc_create_async_r_3d_p2( a, async )
  real, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_create_async_r_3d_p2

subroutine acc_create_async_r_4d_p2( a, async )
  real, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_create_async_r_4d_p2

subroutine acc_create_async_l_1d_p2( a, async )
  logical, dimension(:) :: a
  integer :: async
end subroutine acc_create_async_l_1d_p2

subroutine acc_create_async_l_2d_p2( a, async )
  logical, dimension(:,:) :: a
  integer :: async
end subroutine acc_create_async_l_2d_p2

subroutine acc_create_async_l_3d_p2( a, async )
  logical, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_create_async_l_3d_p2

subroutine acc_create_async_l_4d_p2( a, async )
  logical, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_create_async_l_4d_p2

subroutine acc_create_async_c_1d_p2( a, async )
  character, dimension(:) :: a
  integer :: async
end subroutine acc_create_async_c_1d_p2

subroutine acc_create_async_c_2d_p2( a, async )
  character, dimension(:,:) :: a
  integer :: async
end subroutine acc_create_async_c_2d_p2

subroutine acc_create_async_c_3d_p2( a, async )
  character, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_create_async_c_3d_p2

subroutine acc_create_async_c_4d_p2( a, async )
  character, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_create_async_c_4d_p2

! Automatically generated signatures for acc_create_async
subroutine acc_create_async_i_l_p3( a, len, async )
  integer :: a
  integer :: len
  integer :: async
end subroutine acc_create_async_i_l_p3

subroutine acc_create_async_r_l_p3( a, len, async )
  real :: a
  integer :: len
  integer :: async
end subroutine acc_create_async_r_l_p3

subroutine acc_create_async_l_l_p3( a, len, async )
  logical :: a
  integer :: len
  integer :: async
end subroutine acc_create_async_l_l_p3

subroutine acc_create_async_c_l_p3( a, len, async )
  character :: a
  integer :: len
  integer :: async
end subroutine acc_create_async_c_l_p3

! Automatically generated signatures for acc_copyout
subroutine acc_copyout_i_1d_p1( a )
  integer, dimension(:) :: a
end subroutine acc_copyout_i_1d_p1

subroutine acc_copyout_i_2d_p1( a )
  integer, dimension(:,:) :: a
end subroutine acc_copyout_i_2d_p1

subroutine acc_copyout_i_3d_p1( a )
  integer, dimension(:,:,:) :: a
end subroutine acc_copyout_i_3d_p1

subroutine acc_copyout_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a
end subroutine acc_copyout_i_4d_p1

subroutine acc_copyout_r_1d_p1( a )
  real, dimension(:) :: a
end subroutine acc_copyout_r_1d_p1

subroutine acc_copyout_r_2d_p1( a )
  real, dimension(:,:) :: a
end subroutine acc_copyout_r_2d_p1

subroutine acc_copyout_r_3d_p1( a )
  real, dimension(:,:,:) :: a
end subroutine acc_copyout_r_3d_p1

subroutine acc_copyout_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a
end subroutine acc_copyout_r_4d_p1

subroutine acc_copyout_l_1d_p1( a )
  logical, dimension(:) :: a
end subroutine acc_copyout_l_1d_p1

subroutine acc_copyout_l_2d_p1( a )
  logical, dimension(:,:) :: a
end subroutine acc_copyout_l_2d_p1

subroutine acc_copyout_l_3d_p1( a )
  logical, dimension(:,:,:) :: a
end subroutine acc_copyout_l_3d_p1

subroutine acc_copyout_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a
end subroutine acc_copyout_l_4d_p1

subroutine acc_copyout_c_1d_p1( a )
  character, dimension(:) :: a
end subroutine acc_copyout_c_1d_p1

subroutine acc_copyout_c_2d_p1( a )
  character, dimension(:,:) :: a
end subroutine acc_copyout_c_2d_p1

subroutine acc_copyout_c_3d_p1( a )
  character, dimension(:,:,:) :: a
end subroutine acc_copyout_c_3d_p1

subroutine acc_copyout_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a
end subroutine acc_copyout_c_4d_p1

! Automatically generated signatures for acc_copyout
subroutine acc_copyout_i_l_p2( a, len )
  integer :: a
  integer :: len
end subroutine acc_copyout_i_l_p2

subroutine acc_copyout_r_l_p2( a, len )
  real :: a
  integer :: len
end subroutine acc_copyout_r_l_p2

subroutine acc_copyout_l_l_p2( a, len )
  logical :: a
  integer :: len
end subroutine acc_copyout_l_l_p2

subroutine acc_copyout_c_l_p2( a, len )
  character :: a
  integer :: len
end subroutine acc_copyout_c_l_p2

! Automatically generated signatures for acc_copyout_async
subroutine acc_copyout_async_i_1d_p2( a, async )
  integer, dimension(:) :: a
  integer :: async
end subroutine acc_copyout_async_i_1d_p2

subroutine acc_copyout_async_i_2d_p2( a, async )
  integer, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyout_async_i_2d_p2

subroutine acc_copyout_async_i_3d_p2( a, async )
  integer, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyout_async_i_3d_p2

subroutine acc_copyout_async_i_4d_p2( a, async )
  integer, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyout_async_i_4d_p2

subroutine acc_copyout_async_r_1d_p2( a, async )
  real, dimension(:) :: a
  integer :: async
end subroutine acc_copyout_async_r_1d_p2

subroutine acc_copyout_async_r_2d_p2( a, async )
  real, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyout_async_r_2d_p2

subroutine acc_copyout_async_r_3d_p2( a, async )
  real, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyout_async_r_3d_p2

subroutine acc_copyout_async_r_4d_p2( a, async )
  real, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyout_async_r_4d_p2

subroutine acc_copyout_async_l_1d_p2( a, async )
  logical, dimension(:) :: a
  integer :: async
end subroutine acc_copyout_async_l_1d_p2

subroutine acc_copyout_async_l_2d_p2( a, async )
  logical, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyout_async_l_2d_p2

subroutine acc_copyout_async_l_3d_p2( a, async )
  logical, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyout_async_l_3d_p2

subroutine acc_copyout_async_l_4d_p2( a, async )
  logical, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyout_async_l_4d_p2

subroutine acc_copyout_async_c_1d_p2( a, async )
  character, dimension(:) :: a
  integer :: async
end subroutine acc_copyout_async_c_1d_p2

subroutine acc_copyout_async_c_2d_p2( a, async )
  character, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyout_async_c_2d_p2

subroutine acc_copyout_async_c_3d_p2( a, async )
  character, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyout_async_c_3d_p2

subroutine acc_copyout_async_c_4d_p2( a, async )
  character, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyout_async_c_4d_p2

! Automatically generated signatures for acc_copyout_async
subroutine acc_copyout_async_i_l_p3( a, len, async )
  integer :: a
  integer :: len
  integer :: async
end subroutine acc_copyout_async_i_l_p3

subroutine acc_copyout_async_r_l_p3( a, len, async )
  real :: a
  integer :: len
  integer :: async
end subroutine acc_copyout_async_r_l_p3

subroutine acc_copyout_async_l_l_p3( a, len, async )
  logical :: a
  integer :: len
  integer :: async
end subroutine acc_copyout_async_l_l_p3

subroutine acc_copyout_async_c_l_p3( a, len, async )
  character :: a
  integer :: len
  integer :: async
end subroutine acc_copyout_async_c_l_p3

! Automatically generated signatures for acc_copyout_finalize
subroutine acc_copyout_finalize_i_1d_p1( a )
  integer, dimension(:) :: a
end subroutine acc_copyout_finalize_i_1d_p1

subroutine acc_copyout_finalize_i_2d_p1( a )
  integer, dimension(:,:) :: a
end subroutine acc_copyout_finalize_i_2d_p1

subroutine acc_copyout_finalize_i_3d_p1( a )
  integer, dimension(:,:,:) :: a
end subroutine acc_copyout_finalize_i_3d_p1

subroutine acc_copyout_finalize_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a
end subroutine acc_copyout_finalize_i_4d_p1

subroutine acc_copyout_finalize_r_1d_p1( a )
  real, dimension(:) :: a
end subroutine acc_copyout_finalize_r_1d_p1

subroutine acc_copyout_finalize_r_2d_p1( a )
  real, dimension(:,:) :: a
end subroutine acc_copyout_finalize_r_2d_p1

subroutine acc_copyout_finalize_r_3d_p1( a )
  real, dimension(:,:,:) :: a
end subroutine acc_copyout_finalize_r_3d_p1

subroutine acc_copyout_finalize_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a
end subroutine acc_copyout_finalize_r_4d_p1

subroutine acc_copyout_finalize_l_1d_p1( a )
  logical, dimension(:) :: a
end subroutine acc_copyout_finalize_l_1d_p1

subroutine acc_copyout_finalize_l_2d_p1( a )
  logical, dimension(:,:) :: a
end subroutine acc_copyout_finalize_l_2d_p1

subroutine acc_copyout_finalize_l_3d_p1( a )
  logical, dimension(:,:,:) :: a
end subroutine acc_copyout_finalize_l_3d_p1

subroutine acc_copyout_finalize_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a
end subroutine acc_copyout_finalize_l_4d_p1

subroutine acc_copyout_finalize_c_1d_p1( a )
  character, dimension(:) :: a
end subroutine acc_copyout_finalize_c_1d_p1

subroutine acc_copyout_finalize_c_2d_p1( a )
  character, dimension(:,:) :: a
end subroutine acc_copyout_finalize_c_2d_p1

subroutine acc_copyout_finalize_c_3d_p1( a )
  character, dimension(:,:,:) :: a
end subroutine acc_copyout_finalize_c_3d_p1

subroutine acc_copyout_finalize_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a
end subroutine acc_copyout_finalize_c_4d_p1

! Automatically generated signatures for acc_copyout_finalize
subroutine acc_copyout_finalize_i_l_p2( a, len )
  integer :: a
  integer :: len
end subroutine acc_copyout_finalize_i_l_p2

subroutine acc_copyout_finalize_r_l_p2( a, len )
  real :: a
  integer :: len
end subroutine acc_copyout_finalize_r_l_p2

subroutine acc_copyout_finalize_l_l_p2( a, len )
  logical :: a
  integer :: len
end subroutine acc_copyout_finalize_l_l_p2

subroutine acc_copyout_finalize_c_l_p2( a, len )
  character :: a
  integer :: len
end subroutine acc_copyout_finalize_c_l_p2

! Automatically generated signatures for acc_copyout_finalize_async
subroutine acc_copyout_finalize_async_i_1d_p2( a, async )
  integer, dimension(:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_i_1d_p2

subroutine acc_copyout_finalize_async_i_2d_p2( a, async )
  integer, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_i_2d_p2

subroutine acc_copyout_finalize_async_i_3d_p2( a, async )
  integer, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_i_3d_p2

subroutine acc_copyout_finalize_async_i_4d_p2( a, async )
  integer, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_i_4d_p2

subroutine acc_copyout_finalize_async_r_1d_p2( a, async )
  real, dimension(:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_r_1d_p2

subroutine acc_copyout_finalize_async_r_2d_p2( a, async )
  real, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_r_2d_p2

subroutine acc_copyout_finalize_async_r_3d_p2( a, async )
  real, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_r_3d_p2

subroutine acc_copyout_finalize_async_r_4d_p2( a, async )
  real, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_r_4d_p2

subroutine acc_copyout_finalize_async_l_1d_p2( a, async )
  logical, dimension(:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_l_1d_p2

subroutine acc_copyout_finalize_async_l_2d_p2( a, async )
  logical, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_l_2d_p2

subroutine acc_copyout_finalize_async_l_3d_p2( a, async )
  logical, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_l_3d_p2

subroutine acc_copyout_finalize_async_l_4d_p2( a, async )
  logical, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_l_4d_p2

subroutine acc_copyout_finalize_async_c_1d_p2( a, async )
  character, dimension(:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_c_1d_p2

subroutine acc_copyout_finalize_async_c_2d_p2( a, async )
  character, dimension(:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_c_2d_p2

subroutine acc_copyout_finalize_async_c_3d_p2( a, async )
  character, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_c_3d_p2

subroutine acc_copyout_finalize_async_c_4d_p2( a, async )
  character, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_copyout_finalize_async_c_4d_p2

! Automatically generated signatures for acc_copyout_finalize_async
subroutine acc_copyout_finalize_async_i_l_p3( a, len, async )
  integer :: a
  integer :: len
  integer :: async
end subroutine acc_copyout_finalize_async_i_l_p3

subroutine acc_copyout_finalize_async_r_l_p3( a, len, async )
  real :: a
  integer :: len
  integer :: async
end subroutine acc_copyout_finalize_async_r_l_p3

subroutine acc_copyout_finalize_async_l_l_p3( a, len, async )
  logical :: a
  integer :: len
  integer :: async
end subroutine acc_copyout_finalize_async_l_l_p3

subroutine acc_copyout_finalize_async_c_l_p3( a, len, async )
  character :: a
  integer :: len
  integer :: async
end subroutine acc_copyout_finalize_async_c_l_p3

! Automatically generated signatures for acc_delete
subroutine acc_delete_i_1d_p1( a )
  integer, dimension(:) :: a

end subroutine acc_delete_i_1d_p1

subroutine acc_delete_i_2d_p1( a )
  integer, dimension(:,:) :: a

end subroutine acc_delete_i_2d_p1

subroutine acc_delete_i_3d_p1( a )
  integer, dimension(:,:,:) :: a

end subroutine acc_delete_i_3d_p1

subroutine acc_delete_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a

end subroutine acc_delete_i_4d_p1

subroutine acc_delete_r_1d_p1( a )
  real, dimension(:) :: a

end subroutine acc_delete_r_1d_p1

subroutine acc_delete_r_2d_p1( a )
  real, dimension(:,:) :: a

end subroutine acc_delete_r_2d_p1

subroutine acc_delete_r_3d_p1( a )
  real, dimension(:,:,:) :: a

end subroutine acc_delete_r_3d_p1

subroutine acc_delete_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a

end subroutine acc_delete_r_4d_p1

subroutine acc_delete_l_1d_p1( a )
  logical, dimension(:) :: a

end subroutine acc_delete_l_1d_p1

subroutine acc_delete_l_2d_p1( a )
  logical, dimension(:,:) :: a

end subroutine acc_delete_l_2d_p1

subroutine acc_delete_l_3d_p1( a )
  logical, dimension(:,:,:) :: a

end subroutine acc_delete_l_3d_p1

subroutine acc_delete_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a

end subroutine acc_delete_l_4d_p1

subroutine acc_delete_c_1d_p1( a )
  character, dimension(:) :: a

end subroutine acc_delete_c_1d_p1

subroutine acc_delete_c_2d_p1( a )
  character, dimension(:,:) :: a

end subroutine acc_delete_c_2d_p1

subroutine acc_delete_c_3d_p1( a )
  character, dimension(:,:,:) :: a

end subroutine acc_delete_c_3d_p1

subroutine acc_delete_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a

end subroutine acc_delete_c_4d_p1

! Automatically generated signatures for acc_delete
subroutine acc_delete_i_l_p2( a, len )
  integer :: a
  integer ::  len
end subroutine acc_delete_i_l_p2

subroutine acc_delete_r_l_p2( a, len )
  real :: a
  integer ::  len
end subroutine acc_delete_r_l_p2

subroutine acc_delete_l_l_p2( a, len )
  logical :: a
  integer ::  len
end subroutine acc_delete_l_l_p2

subroutine acc_delete_c_l_p2( a, len )
  character :: a
  integer ::  len
end subroutine acc_delete_c_l_p2

! Automatically generated signatures for acc_delete_async
subroutine acc_delete_async_i_1d_p2( a, async )
  integer, dimension(:) :: a
  integer :: async
end subroutine acc_delete_async_i_1d_p2

subroutine acc_delete_async_i_2d_p2( a, async )
  integer, dimension(:,:) :: a
  integer :: async
end subroutine acc_delete_async_i_2d_p2

subroutine acc_delete_async_i_3d_p2( a, async )
  integer, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_delete_async_i_3d_p2

subroutine acc_delete_async_i_4d_p2( a, async )
  integer, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_delete_async_i_4d_p2

subroutine acc_delete_async_r_1d_p2( a, async )
  real, dimension(:) :: a
  integer :: async
end subroutine acc_delete_async_r_1d_p2

subroutine acc_delete_async_r_2d_p2( a, async )
  real, dimension(:,:) :: a
  integer :: async
end subroutine acc_delete_async_r_2d_p2

subroutine acc_delete_async_r_3d_p2( a, async )
  real, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_delete_async_r_3d_p2

subroutine acc_delete_async_r_4d_p2( a, async )
  real, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_delete_async_r_4d_p2

subroutine acc_delete_async_l_1d_p2( a, async )
  logical, dimension(:) :: a
  integer :: async
end subroutine acc_delete_async_l_1d_p2

subroutine acc_delete_async_l_2d_p2( a, async )
  logical, dimension(:,:) :: a
  integer :: async
end subroutine acc_delete_async_l_2d_p2

subroutine acc_delete_async_l_3d_p2( a, async )
  logical, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_delete_async_l_3d_p2

subroutine acc_delete_async_l_4d_p2( a, async )
  logical, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_delete_async_l_4d_p2

subroutine acc_delete_async_c_1d_p2( a, async )
  character, dimension(:) :: a
  integer :: async
end subroutine acc_delete_async_c_1d_p2

subroutine acc_delete_async_c_2d_p2( a, async )
  character, dimension(:,:) :: a
  integer :: async
end subroutine acc_delete_async_c_2d_p2

subroutine acc_delete_async_c_3d_p2( a, async )
  character, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_delete_async_c_3d_p2

subroutine acc_delete_async_c_4d_p2( a, async )
  character, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_delete_async_c_4d_p2

! Automatically generated signatures for acc_delete_async
subroutine acc_delete_async_i_l_p3( a, len, async )
  integer :: a
  integer :: len
  integer :: async
end subroutine acc_delete_async_i_l_p3

subroutine acc_delete_async_r_l_p3( a, len, async )
  real :: a
  integer :: len
  integer :: async
end subroutine acc_delete_async_r_l_p3

subroutine acc_delete_async_l_l_p3( a, len, async )
  logical :: a
  integer :: len
  integer :: async
end subroutine acc_delete_async_l_l_p3

subroutine acc_delete_async_c_l_p3( a, len, async )
  character :: a
  integer :: len
  integer :: async
end subroutine acc_delete_async_c_l_p3

! Automatically generated signatures for acc_delete_finalize
subroutine acc_delete_finalize_i_1d_p1( a )
  integer, dimension(:) :: a
end subroutine acc_delete_finalize_i_1d_p1

subroutine acc_delete_finalize_i_2d_p1( a )
  integer, dimension(:,:) :: a
end subroutine acc_delete_finalize_i_2d_p1

subroutine acc_delete_finalize_i_3d_p1( a )
  integer, dimension(:,:,:) :: a
end subroutine acc_delete_finalize_i_3d_p1

subroutine acc_delete_finalize_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a
end subroutine acc_delete_finalize_i_4d_p1

subroutine acc_delete_finalize_r_1d_p1( a )
  real, dimension(:) :: a
end subroutine acc_delete_finalize_r_1d_p1

subroutine acc_delete_finalize_r_2d_p1( a )
  real, dimension(:,:) :: a
end subroutine acc_delete_finalize_r_2d_p1

subroutine acc_delete_finalize_r_3d_p1( a )
  real, dimension(:,:,:) :: a
end subroutine acc_delete_finalize_r_3d_p1

subroutine acc_delete_finalize_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a
end subroutine acc_delete_finalize_r_4d_p1

subroutine acc_delete_finalize_l_1d_p1( a )
  logical, dimension(:) :: a
end subroutine acc_delete_finalize_l_1d_p1

subroutine acc_delete_finalize_l_2d_p1( a )
  logical, dimension(:,:) :: a
end subroutine acc_delete_finalize_l_2d_p1

subroutine acc_delete_finalize_l_3d_p1( a )
  logical, dimension(:,:,:) :: a
end subroutine acc_delete_finalize_l_3d_p1

subroutine acc_delete_finalize_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a
end subroutine acc_delete_finalize_l_4d_p1

subroutine acc_delete_finalize_c_1d_p1( a )
  character, dimension(:) :: a
end subroutine acc_delete_finalize_c_1d_p1

subroutine acc_delete_finalize_c_2d_p1( a )
  character, dimension(:,:) :: a
end subroutine acc_delete_finalize_c_2d_p1

subroutine acc_delete_finalize_c_3d_p1( a )
  character, dimension(:,:,:) :: a
end subroutine acc_delete_finalize_c_3d_p1

subroutine acc_delete_finalize_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a
end subroutine acc_delete_finalize_c_4d_p1

! Automatically generated signatures for acc_delete_finalize
subroutine acc_delete_finalize_i_l_p2( a, len )
  integer :: a
  integer :: len
end subroutine acc_delete_finalize_i_l_p2

subroutine acc_delete_finalize_r_l_p2( a, len )
  real :: a
  integer :: len
end subroutine acc_delete_finalize_r_l_p2

subroutine acc_delete_finalize_l_l_p2( a, len )
  logical :: a
  integer :: len
end subroutine acc_delete_finalize_l_l_p2

subroutine acc_delete_finalize_c_l_p2( a, len )
  character :: a
  integer :: len
end subroutine acc_delete_finalize_c_l_p2

! Automatically generated signatures for acc_delete_finalize_async
subroutine acc_delete_finalize_async_i_1d_p2( a, async )
  integer, dimension(:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_i_1d_p2

subroutine acc_delete_finalize_async_i_2d_p2( a, async )
  integer, dimension(:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_i_2d_p2

subroutine acc_delete_finalize_async_i_3d_p2( a, async )
  integer, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_i_3d_p2

subroutine acc_delete_finalize_async_i_4d_p2( a, async )
  integer, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_i_4d_p2

subroutine acc_delete_finalize_async_r_1d_p2( a, async )
  real, dimension(:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_r_1d_p2

subroutine acc_delete_finalize_async_r_2d_p2( a, async )
  real, dimension(:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_r_2d_p2

subroutine acc_delete_finalize_async_r_3d_p2( a, async )
  real, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_r_3d_p2

subroutine acc_delete_finalize_async_r_4d_p2( a, async )
  real, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_r_4d_p2

subroutine acc_delete_finalize_async_l_1d_p2( a, async )
  logical, dimension(:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_l_1d_p2

subroutine acc_delete_finalize_async_l_2d_p2( a, async )
  logical, dimension(:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_l_2d_p2

subroutine acc_delete_finalize_async_l_3d_p2( a, async )
  logical, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_l_3d_p2

subroutine acc_delete_finalize_async_l_4d_p2( a, async )
  logical, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_l_4d_p2

subroutine acc_delete_finalize_async_c_1d_p2( a, async )
  character, dimension(:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_c_1d_p2

subroutine acc_delete_finalize_async_c_2d_p2( a, async )
  character, dimension(:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_c_2d_p2

subroutine acc_delete_finalize_async_c_3d_p2( a, async )
  character, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_c_3d_p2

subroutine acc_delete_finalize_async_c_4d_p2( a, async )
  character, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_delete_finalize_async_c_4d_p2

! Automatically generated signatures for acc_delete_finalize_async
subroutine acc_delete_finalize_async_i_l_p3( a, len, async )
  integer :: a
  integer :: len
  integer :: async
end subroutine acc_delete_finalize_async_i_l_p3

subroutine acc_delete_finalize_async_r_l_p3( a, len, async )
  real :: a
  integer :: len
  integer :: async
end subroutine acc_delete_finalize_async_r_l_p3

subroutine acc_delete_finalize_async_l_l_p3( a, len, async )
  logical :: a
  integer :: len
  integer :: async
end subroutine acc_delete_finalize_async_l_l_p3

subroutine acc_delete_finalize_async_c_l_p3( a, len, async )
  character :: a
  integer :: len
  integer :: async
end subroutine acc_delete_finalize_async_c_l_p3

! Automatically generated signatures for acc_update_device
subroutine acc_update_device_i_1d_p1( a )
  integer, dimension(:) :: a
end subroutine acc_update_device_i_1d_p1

subroutine acc_update_device_i_2d_p1( a )
  integer, dimension(:,:) :: a
end subroutine acc_update_device_i_2d_p1

subroutine acc_update_device_i_3d_p1( a )
  integer, dimension(:,:,:) :: a
end subroutine acc_update_device_i_3d_p1

subroutine acc_update_device_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a
end subroutine acc_update_device_i_4d_p1

subroutine acc_update_device_r_1d_p1( a )
  real, dimension(:) :: a
end subroutine acc_update_device_r_1d_p1

subroutine acc_update_device_r_2d_p1( a )
  real, dimension(:,:) :: a
end subroutine acc_update_device_r_2d_p1

subroutine acc_update_device_r_3d_p1( a )
  real, dimension(:,:,:) :: a
end subroutine acc_update_device_r_3d_p1

subroutine acc_update_device_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a
end subroutine acc_update_device_r_4d_p1

subroutine acc_update_device_l_1d_p1( a )
  logical, dimension(:) :: a
end subroutine acc_update_device_l_1d_p1

subroutine acc_update_device_l_2d_p1( a )
  logical, dimension(:,:) :: a
end subroutine acc_update_device_l_2d_p1

subroutine acc_update_device_l_3d_p1( a )
  logical, dimension(:,:,:) :: a
end subroutine acc_update_device_l_3d_p1

subroutine acc_update_device_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a
end subroutine acc_update_device_l_4d_p1

subroutine acc_update_device_c_1d_p1( a )
  character, dimension(:) :: a
end subroutine acc_update_device_c_1d_p1

subroutine acc_update_device_c_2d_p1( a )
  character, dimension(:,:) :: a
end subroutine acc_update_device_c_2d_p1

subroutine acc_update_device_c_3d_p1( a )
  character, dimension(:,:,:) :: a
end subroutine acc_update_device_c_3d_p1

subroutine acc_update_device_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a
end subroutine acc_update_device_c_4d_p1

! Automatically generated signatures for acc_update_device
subroutine acc_update_device_i_l_p2( a, len )
  integer :: a
  integer :: len
end subroutine acc_update_device_i_l_p2

subroutine acc_update_device_r_l_p2( a, len )
  real :: a
  integer :: len
end subroutine acc_update_device_r_l_p2

subroutine acc_update_device_l_l_p2( a, len )
  logical :: a
  integer :: len
end subroutine acc_update_device_l_l_p2

subroutine acc_update_device_c_l_p2( a, len )
  character :: a
  integer :: len
end subroutine acc_update_device_c_l_p2

! Automatically generated signatures for acc_update_device_async
subroutine acc_update_device_async_i_1d_p2( a, async )
  integer, dimension(:) :: a
  integer :: async
end subroutine acc_update_device_async_i_1d_p2

subroutine acc_update_device_async_i_2d_p2( a, async )
  integer, dimension(:,:) :: a
  integer :: async
end subroutine acc_update_device_async_i_2d_p2

subroutine acc_update_device_async_i_3d_p2( a, async )
  integer, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_update_device_async_i_3d_p2

subroutine acc_update_device_async_i_4d_p2( a, async )
  integer, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_update_device_async_i_4d_p2

subroutine acc_update_device_async_r_1d_p2( a, async )
  real, dimension(:) :: a
  integer :: async
end subroutine acc_update_device_async_r_1d_p2

subroutine acc_update_device_async_r_2d_p2( a, async )
  real, dimension(:,:) :: a
  integer :: async
end subroutine acc_update_device_async_r_2d_p2

subroutine acc_update_device_async_r_3d_p2( a, async )
  real, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_update_device_async_r_3d_p2

subroutine acc_update_device_async_r_4d_p2( a, async )
  real, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_update_device_async_r_4d_p2

subroutine acc_update_device_async_l_1d_p2( a, async )
  logical, dimension(:) :: a
  integer :: async
end subroutine acc_update_device_async_l_1d_p2

subroutine acc_update_device_async_l_2d_p2( a, async )
  logical, dimension(:,:) :: a
  integer :: async
end subroutine acc_update_device_async_l_2d_p2

subroutine acc_update_device_async_l_3d_p2( a, async )
  logical, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_update_device_async_l_3d_p2

subroutine acc_update_device_async_l_4d_p2( a, async )
  logical, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_update_device_async_l_4d_p2

subroutine acc_update_device_async_c_1d_p2( a, async )
  character, dimension(:) :: a
  integer :: async
end subroutine acc_update_device_async_c_1d_p2

subroutine acc_update_device_async_c_2d_p2( a, async )
  character, dimension(:,:) :: a
  integer :: async
end subroutine acc_update_device_async_c_2d_p2

subroutine acc_update_device_async_c_3d_p2( a, async )
  character, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_update_device_async_c_3d_p2

subroutine acc_update_device_async_c_4d_p2( a, async )
  character, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_update_device_async_c_4d_p2

! Automatically generated signatures for acc_update_device_async
subroutine acc_update_device_async_i_l_p3( a, len, async )
  integer :: a
  integer :: len
  integer :: async
end subroutine acc_update_device_async_i_l_p3

subroutine acc_update_device_async_r_l_p3( a, len, async )
  real :: a
  integer :: len
  integer :: async
end subroutine acc_update_device_async_r_l_p3

subroutine acc_update_device_async_l_l_p3( a, len, async )
  logical :: a
  integer :: len
  integer :: async
end subroutine acc_update_device_async_l_l_p3

subroutine acc_update_device_async_c_l_p3( a, len, async )
  character :: a
  integer :: len
  integer :: async
end subroutine acc_update_device_async_c_l_p3

! Automatically generated signatures for acc_update_self
subroutine acc_update_self_i_1d_p1( a )
  integer, dimension(:) :: a
end subroutine acc_update_self_i_1d_p1

subroutine acc_update_self_i_2d_p1( a )
  integer, dimension(:,:) :: a
end subroutine acc_update_self_i_2d_p1

subroutine acc_update_self_i_3d_p1( a )
  integer, dimension(:,:,:) :: a
end subroutine acc_update_self_i_3d_p1

subroutine acc_update_self_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a
end subroutine acc_update_self_i_4d_p1

subroutine acc_update_self_r_1d_p1( a )
  real, dimension(:) :: a
end subroutine acc_update_self_r_1d_p1

subroutine acc_update_self_r_2d_p1( a )
  real, dimension(:,:) :: a
end subroutine acc_update_self_r_2d_p1

subroutine acc_update_self_r_3d_p1( a )
  real, dimension(:,:,:) :: a
end subroutine acc_update_self_r_3d_p1

subroutine acc_update_self_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a
end subroutine acc_update_self_r_4d_p1

subroutine acc_update_self_l_1d_p1( a )
  logical, dimension(:) :: a
end subroutine acc_update_self_l_1d_p1

subroutine acc_update_self_l_2d_p1( a )
  logical, dimension(:,:) :: a
end subroutine acc_update_self_l_2d_p1

subroutine acc_update_self_l_3d_p1( a )
  logical, dimension(:,:,:) :: a
end subroutine acc_update_self_l_3d_p1

subroutine acc_update_self_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a
end subroutine acc_update_self_l_4d_p1

subroutine acc_update_self_c_1d_p1( a )
  character, dimension(:) :: a
end subroutine acc_update_self_c_1d_p1

subroutine acc_update_self_c_2d_p1( a )
  character, dimension(:,:) :: a
end subroutine acc_update_self_c_2d_p1

subroutine acc_update_self_c_3d_p1( a )
  character, dimension(:,:,:) :: a
end subroutine acc_update_self_c_3d_p1

subroutine acc_update_self_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a
end subroutine acc_update_self_c_4d_p1

! Automatically generated signatures for acc_update_self
subroutine acc_update_self_i_l_p2( a, len )
  integer :: a
  integer :: len
end subroutine acc_update_self_i_l_p2

subroutine acc_update_self_r_l_p2( a, len )
  real :: a
  integer :: len
end subroutine acc_update_self_r_l_p2

subroutine acc_update_self_l_l_p2( a, len )
  logical :: a
  integer :: len
end subroutine acc_update_self_l_l_p2

subroutine acc_update_self_c_l_p2( a, len )
  character :: a
  integer :: len
end subroutine acc_update_self_c_l_p2

! Automatically generated signatures for acc_update_self_async
subroutine acc_update_self_async_i_1d_p2( a, async )
  integer, dimension(:) :: a
  integer :: async
end subroutine acc_update_self_async_i_1d_p2

subroutine acc_update_self_async_i_2d_p2( a, async )
  integer, dimension(:,:) :: a
  integer :: async
end subroutine acc_update_self_async_i_2d_p2

subroutine acc_update_self_async_i_3d_p2( a, async )
  integer, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_update_self_async_i_3d_p2

subroutine acc_update_self_async_i_4d_p2( a, async )
  integer, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_update_self_async_i_4d_p2

subroutine acc_update_self_async_r_1d_p2( a, async )
  real, dimension(:) :: a
  integer :: async
end subroutine acc_update_self_async_r_1d_p2

subroutine acc_update_self_async_r_2d_p2( a, async )
  real, dimension(:,:) :: a
  integer :: async
end subroutine acc_update_self_async_r_2d_p2

subroutine acc_update_self_async_r_3d_p2( a, async )
  real, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_update_self_async_r_3d_p2

subroutine acc_update_self_async_r_4d_p2( a, async )
  real, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_update_self_async_r_4d_p2

subroutine acc_update_self_async_l_1d_p2( a, async )
  logical, dimension(:) :: a
  integer :: async
end subroutine acc_update_self_async_l_1d_p2

subroutine acc_update_self_async_l_2d_p2( a, async )
  logical, dimension(:,:) :: a
  integer :: async
end subroutine acc_update_self_async_l_2d_p2

subroutine acc_update_self_async_l_3d_p2( a, async )
  logical, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_update_self_async_l_3d_p2

subroutine acc_update_self_async_l_4d_p2( a, async )
  logical, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_update_self_async_l_4d_p2

subroutine acc_update_self_async_c_1d_p2( a, async )
  character, dimension(:) :: a
  integer :: async
end subroutine acc_update_self_async_c_1d_p2

subroutine acc_update_self_async_c_2d_p2( a, async )
  character, dimension(:,:) :: a
  integer :: async
end subroutine acc_update_self_async_c_2d_p2

subroutine acc_update_self_async_c_3d_p2( a, async )
  character, dimension(:,:,:) :: a
  integer :: async
end subroutine acc_update_self_async_c_3d_p2

subroutine acc_update_self_async_c_4d_p2( a, async )
  character, dimension(:,:,:,:) :: a
  integer :: async
end subroutine acc_update_self_async_c_4d_p2

! Automatically generated signatures for acc_update_self_async
subroutine acc_update_self_async_i_l_p3( a, len, async )
  integer :: a
  integer :: len
  integer :: async
end subroutine acc_update_self_async_i_l_p3

subroutine acc_update_self_async_r_l_p3( a, len, async )
  real :: a
  integer :: len
  integer :: async
end subroutine acc_update_self_async_r_l_p3

subroutine acc_update_self_async_l_l_p3( a, len, async )
  logical :: a
  integer :: len
  integer :: async
end subroutine acc_update_self_async_l_l_p3

subroutine acc_update_self_async_c_l_p3( a, len, async )
  character :: a
  integer :: len
  integer :: async
end subroutine acc_update_self_async_c_l_p3

! Automatically generated signatures for acc_is_present
logical function acc_is_present_i_1d_p1( a )
  integer, dimension(:) :: a
end function acc_is_present_i_1d_p1

logical function acc_is_present_i_2d_p1( a )
  integer, dimension(:,:) :: a
end function acc_is_present_i_2d_p1

logical function acc_is_present_i_3d_p1( a )
  integer, dimension(:,:,:) :: a
end function acc_is_present_i_3d_p1

logical function acc_is_present_i_4d_p1( a )
  integer, dimension(:,:,:,:) :: a
end function acc_is_present_i_4d_p1

logical function acc_is_present_r_1d_p1( a )
  real, dimension(:) :: a
end function acc_is_present_r_1d_p1

logical function acc_is_present_r_2d_p1( a )
  real, dimension(:,:) :: a
end function acc_is_present_r_2d_p1

logical function acc_is_present_r_3d_p1( a )
  real, dimension(:,:,:) :: a
end function acc_is_present_r_3d_p1

logical function acc_is_present_r_4d_p1( a )
  real, dimension(:,:,:,:) :: a
end function acc_is_present_r_4d_p1

logical function acc_is_present_l_1d_p1( a )
  logical, dimension(:) :: a
end function acc_is_present_l_1d_p1

logical function acc_is_present_l_2d_p1( a )
  logical, dimension(:,:) :: a
end function acc_is_present_l_2d_p1

logical function acc_is_present_l_3d_p1( a )
  logical, dimension(:,:,:) :: a
end function acc_is_present_l_3d_p1

logical function acc_is_present_l_4d_p1( a )
  logical, dimension(:,:,:,:) :: a
end function acc_is_present_l_4d_p1

logical function acc_is_present_c_1d_p1( a )
  character, dimension(:) :: a
end function acc_is_present_c_1d_p1

logical function acc_is_present_c_2d_p1( a )
  character, dimension(:,:) :: a
end function acc_is_present_c_2d_p1

logical function acc_is_present_c_3d_p1( a )
  character, dimension(:,:,:) :: a
end function acc_is_present_c_3d_p1

logical function acc_is_present_c_4d_p1( a )
  character, dimension(:,:,:,:) :: a
end function acc_is_present_c_4d_p1

! Automatically generated signatures for acc_is_present
logical function acc_is_present_i_l_p2( a, len )
  integer :: a
  integer :: len
end function acc_is_present_i_l_p2

logical function acc_is_present_r_l_p2( a, len )
  real :: a
  integer :: len
end function acc_is_present_r_l_p2

logical function acc_is_present_l_l_p2( a, len )
  logical :: a
  integer :: len
end function acc_is_present_l_l_p2

logical function acc_is_present_c_l_p2( a, len )
  character :: a
  integer :: len
end function acc_is_present_c_l_p2

end module openacc
