
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for set geographic properties command.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/TransformHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/ModelWrapper.h>

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/TransformPtr.h>

#include <ves/util/commands/Minerva.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

TransformHandler::TransformHandler() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

TransformHandler::~TransformHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  
//
///////////////////////////////////////////////////////////////////////////////

void TransformHandler::Execute ( CommandPtr command, MinervaManager& manager )
{
  ves::open::xml::DataValuePairPtr nodeIDData ( command->GetDataValuePair( "Node ID" ) );
  ves::open::xml::DataValuePairPtr nodeType ( command->GetDataValuePair( "Node Type" ) );
  ves::open::xml::DataValuePairPtr transformData ( command->GetDataValuePair( "Transform" ) );

  std::string nodeId;
  nodeIDData->GetData ( nodeId );

  ves::open::xml::TransformPtr transform ( boost::dynamic_pointer_cast<ves::open::xml::Transform> ( transformData->GetDataXMLObject() ) );

  ModelWrapper::RefPtr modelWrapper ( this->GetOrCreateModel ( nodeId, manager ) );

  ves::open::xml::FloatArrayPtr scaleArray ( transform->GetScaleArray() );
  modelWrapper->scale ( osg::Vec3d ( scaleArray->GetElement ( 0 ), scaleArray->GetElement ( 1 ), scaleArray->GetElement ( 2 ) ) );

  // TODO: Handle translation and rotation from cad transform.

  manager.UpdateModel ( modelWrapper.get() );
}
