
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for add earth command.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/NavigateToModel.h>
#include <ves/xplorer/minerva/MinervaManager.h>
#include <ves/xplorer/minerva/ModelWrapper.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <Minerva/Core/Data/Camera.h>

#include <gmtl/Math.h>
#include <gmtl/Generate.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

NavigateToModel::NavigateToModel() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

NavigateToModel::~NavigateToModel()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Navigate to the model.  Implementation modified from ves::xplorer::event::cad::NavigateToEventHandler
//
///////////////////////////////////////////////////////////////////////////////

void NavigateToModel::Execute ( CommandPtr command, MinervaManager& manager )
{
  ves::open::xml::DataValuePairPtr activeModelDVP ( command->GetDataValuePair( "NAVIGATE_TO" ) );

  if( !activeModelDVP )
  {
    return;
  }

  std::string nodeId;
  activeModelDVP->GetData ( nodeId );

  ModelWrapper::RefPtr modelWrapper ( 0x0 );
  if ( manager.HasModel ( nodeId ) )
  {
    modelWrapper = manager.GetModel ( nodeId );
  }

  if ( !modelWrapper.valid() )
    return;

  ves::xplorer::scenegraph::CADEntity *cadEntity ( modelWrapper->GetCADEntity() );
  if ( 0x0 == cadEntity )
    return;

  ves::xplorer::scenegraph::DCS *dcs ( cadEntity->GetDCS() );
  if ( 0x0 == dcs )
    return;

  osg::BoundingSphere boundingSphere ( dcs->getBound() );
  osg::Vec3d location ( modelWrapper->location() );
  const double altitudeOffset ( gmtl::Math::Max( double( boundingSphere.radius() * 2 ), 3000.0 ) );

  Minerva::Core::Data::Camera::RefPtr camera ( new Minerva::Core::Data::Camera );
  camera->longitude ( location[0] );
  camera->latitude ( location[1] );
  camera->altitude ( location[2] + altitudeOffset );

  gmtl::Matrix44d viewMatrix;
  manager.GetViewMatrix ( camera.get(), viewMatrix );

  gmtl::Quat<double> rotation; gmtl::setRot ( rotation, viewMatrix );
  gmtl::Vec3d translate; gmtl::setTrans ( translate, viewMatrix );

  /// Tell the animation engine to set the world dcs.
  ves::xplorer::NavigationAnimationEngine::instance()->SetDCS(
    ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS() );
    
  /// Tell the animation engine where to go.
  ves::xplorer::NavigationAnimationEngine::instance()->SetAnimationEndPoints(
    translate, rotation, true, dcs );
}
