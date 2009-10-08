
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for adding a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/AddRasterGroupHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>

#include <Minerva/Core/Layers/RasterLayer.h>

#include <ves/open/xml/Command.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

AddRasterGroupHandler::AddRasterGroupHandler() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

AddRasterGroupHandler::~AddRasterGroupHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Add the earth.
//
///////////////////////////////////////////////////////////////////////////////

void AddRasterGroupHandler::Execute ( CommandPtr command, MinervaManager& manager )
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
          manager.AddRasterLayer ( layer.get() );
        }
      }
    }
  }
}
