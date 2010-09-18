/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
  const double altitudeOffset ( gmtl::Math::Max( double( boundingSphere.radius() * 2 ), 1000.0 ) );

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
    ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
    
  /// Tell the animation engine where to go.
  ves::xplorer::NavigationAnimationEngine::instance()->SetAnimationEndPoints(
    translate, rotation, true, dcs );
}
