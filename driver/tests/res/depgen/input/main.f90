#include "1.inc"

module same_file_mod
    use xmod_only_mod
    use other_file_mod
end module same_file_mod

program p1
    use inc_mod
    use same_file_mod
    use iso_c_binding
end program p1
