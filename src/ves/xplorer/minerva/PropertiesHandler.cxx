
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for set geographic properties command.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/PropertiesHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/ModelWrapper.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/util/commands/Minerva.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

PropertiesHandler::PropertiesHandler() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

PropertiesHandler::~PropertiesHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  
//
///////////////////////////////////////////////////////////////////////////////

void PropertiesHandler::Execute ( CommandPtr command, MinervaManager& manager )
{
  ves::open::xml::DataValuePairPtr nodeIDData ( command->GetDataValuePair( "Node ID" ) );
  ves::open::xml::DataValuePairPtr nodeTypeData ( command->GetDataValuePair( "Node Type" ) );

  ves::open::xml::DataValuePairPtr longitudeData ( command->GetDataValuePair ( ves::util::commands::LONGITUDE_VALUE ) );
  ves::open::xml::DataValuePairPtr latitudeData ( command->GetDataValuePair ( ves::util::commands::LATITUDE_VALUE ) );

  // Note: node id is a guid.
  std::string nodeId;
  nodeIDData->GetData ( nodeId );

  double longitude ( 0.0 );
  longitudeData->GetData ( longitude );

  double latitude ( 0.0 );
  latitudeData->GetData ( latitude );

  ModelWrapper::RefPtr modelWrapper ( this->GetOrCreateModel ( nodeId, manager ) );
  modelWrapper->location ( osg::Vec3d ( longitude, latitude, 0.0 ) );
  manager.UpdateModel ( modelWrapper.get() );
}
