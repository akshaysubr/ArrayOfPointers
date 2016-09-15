cmake_minimum_required(VERSION 2.8)
project(ArrayOfPointers)

enable_language(Fortran)

set( CMAKE_MODULE_PATH ${PadeOps_SOURCE_DIR} )

if (NOT CMAKE_BUILD_TYPE)
    message(STATUS "No build type selected, default to Release")
    set(CMAKE_BUILD_TYPE "Release")
endif()

set(CMAKE_Fortran_COMPILER_ID $ENV{COMPILER_ID})

# Intel compiler suite
if ( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
    if ( CMAKE_BUILD_TYPE MATCHES "Release" )
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O5 -mkl -warn all -xhost -dynamic -qopt-report=2 -qopt-report-phase=vec")
    elseif ( CMAKE_BUILD_TYPE MATCHES "Debug" )
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -pg -mkl -check all -check noarg-temp-created -fpe0 -warn -traceback -debug extended -assume realloc_lhs -fstack-protector -assume protect_parens -implicitnone")
    endif()

# GNU compiler on OSX
elseif ( CMAKE_Fortran_COMPILER_ID MATCHES "GNU_OSX")
    if ( CMAKE_BUILD_TYPE MATCHES "Release" )
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -llapack -Wall -Wconversion -Wextra -Waliasing -ffree-form -ffree-line-length-none -ffast-math -funroll-loops -fno-protect-parens -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")
    elseif ( CMAKE_BUILD_TYPE MATCHES "Debug" )
        set(CMAKE_Fortran_FLAGS "-Og -g -pg -llapack -ffree-form -ffree-line-length-none -fbounds-check -ffpe-trap=zero,overflow,underflow -Wall -Wconversion -Wextra -Waliasing -Wsurprising -fbacktrace -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")
    endif()

# Standard GNU compilers
elseif ( CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    if ( CMAKE_BUILD_TYPE MATCHES "Release" )
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -llapack -Wall -Wconversion -Wextra -Waliasing -ffree-form -ffree-line-length-none -ffast-math -march=native -funroll-loops -fno-protect-parens")
    elseif ( CMAKE_BUILD_TYPE MATCHES "Debug" )
        set(CMAKE_Fortran_FLAGS "-Og -g -pg -llapack -ffree-form -ffree-line-length-none -fcheck=all -fbounds-check -ffpe-trap=zero,overflow,underflow -Wall -Wconversion -Wextra -Waliasing -Wsurprising -fbacktrace")
    endif()
endif()

# Enable testing
enable_testing()

# Add source file directories
add_subdirectory(src)
