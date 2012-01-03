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
//  Base event handler for Minerva commands.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/EventHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/ModelWrapper.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/util/commands/Minerva.h>

#include <Minerva/Core/Functions/ReadFile.h>
#include <Minerva/Core/Layers/RasterLayerWms.h>

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

EventHandler::EventHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

EventHandler::~EventHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Get or create the model.
//
///////////////////////////////////////////////////////////////////////////////

ModelWrapper* EventHandler::GetOrCreateModel ( const std::string& nodeId, MinervaManager& manager )
{
  ModelWrapper::RefPtr modelWrapper ( 0x0 );
  if ( manager.HasModel ( nodeId ) )
  {
    modelWrapper = manager.GetModel ( nodeId );
  }
  else
  {
    const unsigned int numModels ( ves::xplorer::ModelHandler::instance()->GetNumberOfModels() );
    for ( unsigned int i = 0; i < numModels; ++i )
    {
      ves::xplorer::Model *model ( ves::xplorer::ModelHandler::instance()->GetModel ( i ) );
      if ( 0x0 != model )
      {
        ves::xplorer::ModelCADHandler *modelHandler ( model->GetModelCADHandler() );
        if ( 0x0 != modelHandler )
        {
          ves::xplorer::scenegraph::CADEntity* tempPart ( modelHandler->GetPart ( nodeId ) );
          if ( 0x0 != tempPart )
          {
            modelWrapper = new ModelWrapper;

            modelWrapper->SetCADEntity ( tempPart );
            manager.AddModel ( nodeId, modelWrapper.get() );

            // For debugging placement.
#if 0
            modelWrapper->scale ( osg::Vec3d ( 100000.0, 100000.0, 100000.0 ) );
#endif

          }
        }
      }
    }
  }

  return modelWrapper.release();
}
///////////////////////////////////////////////////////////////////////////////
EventHandler::RasterLayer* EventHandler::_createRasterLayerFromCommand ( CommandPtr command )
{
  ves::open::xml::DataValuePairPtr typeData ( command->GetDataValuePair ( ves::util::names::LAYER_DATA_SOURCE ) );
  
  if ( typeData )
  {
    std::string type;
    typeData->GetData ( type );

    if ( ves::util::values::FILESYSTEM_SOURCE == type )
    {
      return EventHandler::_createFileSystemLayerFromCommand ( command );
    }
    else if ( ves::util::values::WMS_SOURCE == type )
    {
      return EventHandler::_createWMSLayerFromCommand ( command );
    }
    else
    {
      vprDEBUG( vesDBG, 0 ) << "|\tCannot create layer of type " << type << std::endl << vprDEBUG_FLUSH;
    }
  }
  
  vprDEBUG( vesDBG, 0 ) << "|\tCannot determine type of layer. " << std::endl << vprDEBUG_FLUSH;
  return 0x0;
}
///////////////////////////////////////////////////////////////////////////////
EventHandler::RasterLayer* EventHandler::_createWMSLayerFromCommand ( CommandPtr command )
{
  ves::open::xml::DataValuePairPtr guidData ( command->GetDataValuePair ( ves::util::names::UNIQUE_ID ) );
  ves::open::xml::DataValuePairPtr serverData ( command->GetDataValuePair ( ves::util::names::SERVER_URL ) );
  ves::open::xml::DataValuePairPtr formatData ( command->GetDataValuePair ( ves::util::names::WMS_FORMAT ) );
  ves::open::xml::DataValuePairPtr layersData ( command->GetDataValuePair ( ves::util::names::WMS_LAYERS ) );
  ves::open::xml::DataValuePairPtr stylesData ( command->GetDataValuePair ( ves::util::names::WMS_STYLES ) );

  if ( guidData && serverData && formatData && layersData && stylesData )
  {
    std::string guid;
    guidData->GetData ( guid );

    std::string server;
    serverData->GetData ( server );

    std::string format;
    formatData->GetData ( format );

    std::string layers;
    layersData->GetData ( layers );

    std::string styles;
    stylesData->GetData ( styles );

    typedef Minerva::Core::Layers::RasterLayerWms RasterLayerWms;
    typedef RasterLayerWms::Options Options;
    typedef RasterLayerWms::Extents Extents;

    Extents extents ( -180.0, -90.0, 180.0, 90.0 );
    Options options;
    options["format"] = format;
    options["layers"] = layers;
    options["styles"] = styles;
    options["request"] = "GetMap";
    options["service"] = "WMS";
    options["srs"] = "EPSG:4326";
    options["version"] = "1.1.1";

    std::string::iterator i ( std::find ( server.begin(), server.end(), '?' ) );
    if ( server.end() != i )
    {
      // Split the sub-string to the right of the '?'
      typedef std::vector<std::string> StringList;
      StringList arguments;
      boost::algorithm::split ( arguments, server, boost::algorithm::is_any_of ( "&" ) );
      for ( StringList::const_iterator j = arguments.begin(); j != arguments.end(); ++j )
      {
        StringList argument;
        boost::algorithm::split ( argument, *j, boost::algorithm::is_any_of ( "=" ) );
        if ( 2 == argument.size() )
        {
          options[argument.at(0)] = argument.at(1);
        }
      }

      // Reset the server url.
      server = std::string ( server.begin(), i );
    }

    RasterLayerWms::RefPtr layer ( new RasterLayerWms ( extents, server, options ) );
    layer->objectId ( guid );

    vprDEBUG( vesDBG, 0 ) << "|\tCreating WMS layer with " << std::endl 
      << "Server: " << server  << std::endl 
      << "Layers: " << layers  << std::endl 
      << "Styles: " << styles  << std::endl 
      << "Format: " << format  << std::endl << vprDEBUG_FLUSH;

    return layer.release();
  }

  return 0x0;
}
///////////////////////////////////////////////////////////////////////////////
EventHandler::RasterLayer* EventHandler::_createFileSystemLayerFromCommand ( CommandPtr command )
{
  ves::open::xml::DataValuePairPtr guidData ( command->GetDataValuePair ( ves::util::names::UNIQUE_ID ) );
  ves::open::xml::DataValuePairPtr filenameData ( command->GetDataValuePair ( ves::util::names::FILENAME ) );

  if ( guidData && filenameData )
  {
    std::string guid;
    guidData->GetData ( guid );

    std::string filename;
    filenameData->GetData ( filename );

    Minerva::Core::Data::Feature::RefPtr feature ( Minerva::Core::Functions::readFile ( filename ) );
    RasterLayer::RefPtr rasterLayer ( dynamic_cast<RasterLayer*> ( feature.get() ) );

    if ( rasterLayer.valid() )
    {
      vprDEBUG( vesDBG, 0 ) << "|\tCreating layer from file " << filename << std::endl << vprDEBUG_FLUSH;

      feature = 0x0;

      rasterLayer->objectId ( guid );
      return rasterLayer.release();
    }
  }

  return 0x0;
}
