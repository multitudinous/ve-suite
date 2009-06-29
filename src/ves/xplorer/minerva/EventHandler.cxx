
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

#include <Minerva/Core/Layers/RasterLayerWms.h>

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

            // ves units are in feet.  Add the conversion to meters.
            modelWrapper->toMeters ( 0.3048 );
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
//
//  Create a raster layer from the command.
//
///////////////////////////////////////////////////////////////////////////////

EventHandler::RasterLayer* EventHandler::_createRasterLayerFromCommand ( CommandPtr command )
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

    RasterLayerWms::RefPtr layer ( new RasterLayerWms ( extents, server, options ) );
    layer->objectId ( guid );

    vprDEBUG( vesDBG, 0 ) << "|Creating WMS layer with " << std::endl 
      << "Server: " << server  << std::endl 
      << "Layers: " << layers  << std::endl 
      << "Styles: " << styles  << std::endl 
      << "Format: " << format  << std::endl << vprDEBUG_FLUSH;

    return layer.release();
  }
  
  return 0x0;
}
