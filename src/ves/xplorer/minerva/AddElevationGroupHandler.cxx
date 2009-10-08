
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for adding a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/AddElevationGroupHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>

#include <Minerva/Core/Layers/RasterLayer.h>

#include <ves/open/xml/Command.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

AddElevationGroupHandler::AddElevationGroupHandler() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

AddElevationGroupHandler::~AddElevationGroupHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Add the earth.
//
///////////////////////////////////////////////////////////////////////////////

void AddElevationGroupHandler::Execute ( CommandPtr command, MinervaManager& manager )
{
  if ( command )
  {
    const std::size_t numLayers ( command->GetNumberOfDataValuePairs() );
    for ( std::size_t i = 0; i < numLayers; ++i )
    {
      ves::open::xml::DataValuePairPtr dvp ( command->GetDataValuePair ( i ) );
      if ( dvp )
      {
        CommandPtr commandForLayer ( boost::dynamic_pointer_cast<ves::open::xml::Command> ( dvp->GetDataXMLObject() ) );
        Minerva::Core::Layers::RasterLayer::RefPtr layer ( EventHandler::_createRasterLayerFromCommand ( commandForLayer ) );
        if ( layer.valid() )
        {
          manager.AddElevationLayer ( layer.get() );
        }
      }
    }
  }
}
