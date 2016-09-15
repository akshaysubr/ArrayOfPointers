# Add executable ArrayOfPointers
add_executable( ArrayOfPointers test_arrayofpointers.F90 )

# Test the executable ArrayOfPointers
add_test(NAME test_ArrayOfPointers WORKING_DIRECTORY ${ArrayOfPointers_BINARY_DIR}/src COMMAND ArrayOfPointers)
#add_test( test_ArrayOfPointers ${ArrayOfPointers_BINARY_DIR}/src/ArrayOfPointers )
