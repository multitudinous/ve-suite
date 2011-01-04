/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

  Minerva::Common::Extents extents ( layer->extents() );
  Usul::Math::Vec2d center ( extents.center() );
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
    ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
    
  /// Tell the animation engine where to go.
  ves::xplorer::NavigationAnimationEngine::instance()->SetAnimationEndPoints(
    translate, rotation, true, ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
}
