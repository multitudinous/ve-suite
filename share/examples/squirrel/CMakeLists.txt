install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION ${shareRoot}/examples
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING
    PATTERN "*.nut"
)
