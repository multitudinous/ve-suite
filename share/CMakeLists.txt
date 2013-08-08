set(shareRoot share/vesuite)

set( rootDataFiles
    ptconfigfile.txt
    Draft2.png
    Info_Panel.png
    Info_Panel-No_Title.png
    splotch.png
    ves3-cluster-config.example )
install(FILES ${rootDataFiles} DESTINATION ${shareRoot})

file(GLOB audioDataFiles "audio/*.wav")
install(FILES ${audioDataFiles} DESTINATION ${shareRoot})

install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/share/fonts
    DESTINATION ${shareRoot}
    USE_SOURCE_PERMISSIONS
    COMPONENT vesuite-dev
    FILES_MATCHING PATTERN "*.ttf"
    PATTERN ".svn" EXCLUDE
    PATTERN ".DS_Store" EXCLUDE
)

#install(
#    DIRECTORY ${PROJECT_SOURCE_DIR}/share
#    DESTINATION ${shareRoot}
#    USE_SOURCE_PERMISSIONS
#    COMPONENT vesuite-dev
#    FILES_MATCHING PATTERN "audio/*.wav"
#    PATTERN "CMakeLists.txt" EXCLUDE
#    PATTERN ".svn" EXCLUDE
#    PATTERN ".DS_Store" EXCLUDE
#)

