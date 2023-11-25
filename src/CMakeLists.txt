# explicitly name all files in src/ directory
set(MODULE_FILES 
    constants_m.f90
    math_m.f90
    random_m.f90
   )

# compile source files to lib_modules
add_library(lib_modules STATIC ${MODULE_FILES})

# compile tests executables
add_executable(alife main.f90)
target_link_libraries(alife lib_modules)
