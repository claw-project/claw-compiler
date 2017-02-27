# List of test directories to be executed
set(TEST_SET loops)
add_subdirectory(extract1)
add_subdirectory(extract2)
add_subdirectory(extract3)
add_subdirectory(extract4)
add_subdirectory(extract5)
add_subdirectory(extract6)
add_subdirectory(extract7)     # with target clause
add_subdirectory(fusion1)
add_subdirectory(fusion2)
add_subdirectory(fusion3)
add_subdirectory(fusion4)
add_subdirectory(fusion5)
add_subdirectory(fusion6)
add_subdirectory(fusion7)      # with target clause
add_subdirectory(fusion8)      # impossible fusion because of assignment
add_subdirectory(hoist1)
add_subdirectory(hoist2)
add_subdirectory(hoist3)       # with target clause
add_subdirectory(interchange1)
add_subdirectory(interchange2)
add_subdirectory(interchange3) # with target clause
