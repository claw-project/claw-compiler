! @author skosukhin
! @brief  This file is used as a signatutre only module to generate .xmod file.
!         These files are needed during a full parse with the OMNI Compiler.

module ifcore

  interface
    subroutine tracebackqq(string, user_exit_code, status, eptr)
      character*(*), intent(in), optional :: string
      integer(4), intent(in), optional :: user_exit_code
      integer(4), intent(out), optional :: status
! eptr is of type POINTER - Integer:
! https://software.intel.com/content/www/us/en/develop/documentation/fortran-compiler-developer-guide-and-reference/top/language-reference/a-to-z-reference/o-to-p/pointer-integer.html
#if defined(__x86_64) || defined(__x86_64__)
      integer(8), optional :: eptr
#else
      integer(4), optional :: eptr
#endif
    end subroutine
  end interface

end module
