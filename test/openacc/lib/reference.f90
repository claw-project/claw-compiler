module test_openacc_lib

contains

  subroutine sub1()
    use openacc
    integer :: local_device


    call acc_set_device_num(local_device, acc_device_nvidia)
  end subroutine sub1

end module test_openacc_lib
