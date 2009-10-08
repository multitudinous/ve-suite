
///////////////////////////////////////////////////////////////////////////////
//
//  Constants for Minerva commands and event handlers.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_UTIL_COMMANDS_MINERVA_H
#define VES_UTIL_COMMANDS_MINERVA_H

#include <string>

namespace ves {
namespace util {
namespace commands {


  const std::string ADD_EARTH_COMMAND_NAME ( "Add Earth" );
  const std::string REMOVE_EARTH_COMMAND_NAME ( "Remove Earth" );
  const std::string SET_GEOGRAPHIC_PROPERTIERS ( "Set Geographic Properties" );

  const std::string ADD_ELEVATION_LAYER ( "Add Elevation Layer" );
  const std::string ADD_ELEVATION_GROUP ( "Add Elevation Group" );
  const std::string ADD_RASTER_LAYER ( "Add Raster Layer" );
  const std::string ADD_RASTER_GROUP ( "Add Raster Group" );

  const std::string REMOVE_ELEVATION_LAYER ( "Remove Elevation Layer" );
  const std::string REMOVE_RASTER_LAYER ( "Remove Raster Layer" );

}

namespace names {

  const std::string LONGITUDE_VALUE ( "Longitude" );
  const std::string LATITUDE_VALUE ( "Latitude" );

  const std::string LAYER_DATA_SOURCE ( "Layer Data Source" );
  const std::string UNIQUE_ID ( "Unique Id" );
  const std::string WMS_OPTIONS ( "WMS Options" );
  const std::string WMS_FORMAT ( "format" );
  const std::string WMS_LAYERS ( "layers" );
  const std::string WMS_STYLES ( "styles" );
  const std::string SERVER_URL ( "Server URL" );

  const std::string FILENAME ( "filename" );

}

namespace values {

  const std::string WMS_SOURCE ( "WMS Source" );
  const std::string FILESYSTEM_SOURCE ( "Filesystem Source" );
}

}
}

#endif // VES_UTIL_COMMANDS_MINERVA_H
