set(shareRoot share/vesuite)

# Root share directory files
set( rootDataFiles
    ptconfigfile.txt
    Draft2.png
    Info_Panel.png
    Info_Panel-No_Title.png
    splotch.png
    ves3-cluster-config.example )
install(FILES ${rootDataFiles} DESTINATION ${shareRoot})

# Audio files
file(GLOB audioDataFiles "audio/*.wav")
install(FILES ${audioDataFiles} DESTINATION ${shareRoot})

# Font files
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/share/fonts
    DESTINATION ${shareRoot}
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING PATTERN "*.ttf"
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
)

# OpenGL shader files
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/share/glsl
    DESTINATION ${shareRoot}
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING 
    PATTERN "*.glsl"
    PATTERN "*.fs"
    PATTERN "*.vs"
    PATTERN "CMakeLists.txt" EXCLUDE
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
)

# vea files
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/share/shaders
    DESTINATION ${shareRoot}
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING 
    PATTERN "*.vea"
    PATTERN "CMakeLists.txt" EXCLUDE
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
)

# vrj_config files
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/share/vrj_configs
    DESTINATION ${shareRoot}
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING 
    PATTERN "*.jconf"
    PATTERN "CMakeLists.txt" EXCLUDE
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
)

# logo files
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/share/logo
    DESTINATION ${shareRoot}
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING 
    PATTERN "*.ive"
    PATTERN "CMakeLists.txt" EXCLUDE
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
)

# Character files
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/share/osg-data
    DESTINATION ${shareRoot}
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING 
    PATTERN "*.ive"
    PATTERN "*.osg"
    PATTERN "*.jpg"
    PATTERN "CMakeLists.txt" EXCLUDE
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
)

# Textures for the osgEphemeris library
install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/osg-textures/
    DESTINATION ${shareRoot}
    FILES_MATCHING 
    PATTERN "*.jpg"
    PATTERN "*.rgb"
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
)

add_subdirectory(examples)
#add_subdirectory(tools)

# icons and .desktop files for Linux (ignore everything else for now)
install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/scripts
    DESTINATION ${shareRoot}
    FILES_MATCHING
    PATTERN "*.png"
    PATTERN "*.xml"
    PATTERN "*.desktop"
    PATTERN ".DS_Store" EXCLUDE
)

