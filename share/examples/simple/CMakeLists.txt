set(shareRoot share/vesuite)

# vtk and ves file files
install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION ${shareRoot}/examples
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING 
    PATTERN "*"
    PATTERN "CMakeLists.txt" EXCLUDE
    PATTERN "SConscript" EXCLUDE
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
    PATTERN "tt" EXCLUDE
)
