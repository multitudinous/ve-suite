/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

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

  const std::string NAVIGATE_TO_LAYER ( "Navigate To Layer" );

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
