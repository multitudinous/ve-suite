
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for add earth command.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/NavigateToLayer.h>
#include <ves/xplorer/minerva/MinervaManager.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/util/commands/Minerva.h>

#include <Minerva/Core/Data/Camera.h>
#include <Minerva/Core/Layers/RasterLayer.h>

#include <gmtl/Math.h>
#include <gmtl/Generate.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

NavigateToLayer::NavigateToLayer() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

NavigateToLayer::~NavigateToLayer()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Navigate to the layer.  Implementation modified from ves::xplorer::event::cad::NavigateToEventHandler
//
///////////////////////////////////////////////////////////////////////////////

void NavigateToLayer::Execute ( CommandPtr command, MinervaManager& manager )
{
  ves::open::xml::DataValuePairPtr guidDVP ( command->GetDataValuePair( ves::util::names::UNIQUE_ID ) );

  if( !guidDVP )
  {
    return;
  }

  std::string layerId;
  guidDVP->GetData( layerId );

  Minerva::Core::Layers::RasterLayer::RefPtr layer ( manager.GetLayer( layerId ) );
  if ( !layer )
  {
    std::cout << "Could not find layer." << std::endl;
    return;
  }

  Minerva::Core::Extents<osg::Vec2d> extents ( layer->extents() );
  osg::Vec2d center ( extents.center() );
  const double diameter ( 2.0 * osg::PI * 6378137.0 );
  const double metersPerDegree ( diameter / 360.0 );
  const double length ( ( extents.maximum() - extents.minimum() ).length() );
  const double altitude ( length * metersPerDegree );

  Minerva::Core::Data::Camera::RefPtr camera ( new Minerva::Core::Data::Camera );
  camera->longitude ( center[0] );
  camera->latitude ( center[1] );
  camera->altitude ( altitude );

  gmtl::Matrix44d viewMatrix;
  manager.GetViewMatrix ( camera.get(), viewMatrix );

  gmtl::Quat<double> rotation; gmtl::setRot ( rotation, viewMatrix );
  gmtl::Vec3d translate; gmtl::setTrans ( translate, viewMatrix );

  /// Tell the animation engine to set the world dcs.
  ves::xplorer::NavigationAnimationEngine::instance()->SetDCS(
    ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS() );
    
  /// Tell the animation engine where to go.
  ves::xplorer::NavigationAnimationEngine::instance()->SetAnimationEndPoints(
    translate, rotation, true, ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS() );
}
