SN_ADD_EXECUTABLE( spike_wxhello wxhello.cpp )
ADD_DEFINITIONS( ${wxWidgets_DEFINITIONS} )
SN_LINK_LIBRARIES( spike_wxhello ${wxWidgets_LIBRARIES} )
