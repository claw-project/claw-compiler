# List of test directories to be executed
add_subdirectory(array1) # simple transformation with 1 dimension
add_subdirectory(array2) # simple transformation with 2 dimension
#add_subdirectory(array3) # on hold for the moment
add_subdirectory(array4) # Block transformation without dependency
add_subdirectory(array5) # Block transformation without dependency, allocatable
add_subdirectory(array6) # Lower/upper bounds with offset
add_subdirectory(array7) # Induction clause
add_subdirectory(array8) # Block transformation (test array refs update)
#add_subdirectory(array9) # Block transformation with dependency
add_subdirectory(continuation)
