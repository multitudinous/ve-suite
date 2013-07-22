FIND_PATH(Sqrat_INCLUDE_DIR sqrat.h 
    PATHS ${Sqrat_ROOT}
        ENV Sqrat_ROOT 
    PATH_SUFFIXES include)

IF(Sqrat_INCLUDE_DIR)
   SET(Sqrat_FOUND TRUE)
ENDIF(Sqrat_INCLUDE_DIR)

IF(Sqrat_FOUND)
   IF (NOT Sqrat_FIND_QUIETLY)
	   MESSAGE(STATUS "Found Sqrat: ${Sqrat_INCLUDE_DIR}")
   ENDIF(NOT Sqrat_FIND_QUIETLY)
ELSE(Sqrat_FOUND)
   IF (Sqrat_FIND_REQUIRED)
      MESSAGE(FATAL_ERROR "Could not find Sqrat")
   ENDIF(Sqrat_FIND_REQUIRED)
ENDIF(Sqrat_FOUND)

mark_as_advanced( FORCE
    Sqrat_INCLUDE_DIR
)