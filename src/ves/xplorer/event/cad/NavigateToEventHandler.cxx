/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
// --- VE-Suite Includes --- //
#include <ves/xplorer/event/cad/NavigateToEventHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/environment/NavigationAnimationEngine.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/xplorer/Debug.h>

#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/Group>

// --- C/C++ Libraries --- //
#include <string>

using namespace ves::xplorer::event;
using namespace ves::xplorer::event::cad;
using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
using namespace ves::open::xml;

using namespace ves::xplorer;

namespace vx = ves::xplorer;
namespace vxs = vx::scenegraph;

////////////////////////////////////////////////////////////////////////////////
NavigateToEventHandler::NavigateToEventHandler()
        :
        ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NavigateToEventHandler::~NavigateToEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NavigateToEventHandler::NavigateToEventHandler( const NavigateToEventHandler& rhs )
        :
        ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NavigateToEventHandler& NavigateToEventHandler::operator=( const NavigateToEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void NavigateToEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void NavigateToEventHandler::Execute( const ves::open::xml::XMLObjectPtr& xmlObject )
{
    CommandPtr command( 
        boost::dynamic_pointer_cast< ves::open::xml::Command >( xmlObject ) );
    DataValuePairPtr activeModelDVP =
        command->GetDataValuePair( "NAVIGATE_TO" );

    if( !activeModelDVP )
    {
        return;
    }

    std::string viewData;
    activeModelDVP->GetData( viewData );

    DataValuePairPtr selectDVP = command->GetDataValuePair( "Select" );
    std::string selectMethod = "Default";
    if( selectDVP )
    {
        selectMethod = "Glow";
    }
    
    SkyCamTo( viewData, selectMethod );
}
////////////////////////////////////////////////////////////////////////////////
void NavigateToEventHandler::SkyCamTo( const std::string& viewData, const std::string& selectMethod )
{
    if( !ModelHandler::instance()->GetActiveModel() )
    {
        return;
    }
    
    //To make this work we must:
    //1. get current state of world dcs
    //2. set dcs back to zero.
    //3. figure out where to set the center of the world
    //4. set the new rotation
    //5. get the vector of the this location
    //6. hand of to nav animation engine
    //7. reset the world dcs back to original state
    //Unselect the previous selected DCS
    vx::DeviceHandler::instance()->UnselectObjects();
    vx::ModelCADHandler* mcadHandler = vx::ModelHandler::instance()->
        GetActiveModel()->GetModelCADHandler();
    osg::ref_ptr< vxs::DCS > selectedDCS;
    if( viewData == vx::ModelHandler::instance()->GetActiveModel()->GetID() )
    {
        //get the selected plugins cad
        //highlight it.
        std::string rootID = mcadHandler->GetRootCADNodeID();
            
        selectedDCS = mcadHandler->GetAssembly( rootID );
    }
    else if( mcadHandler->GetAssembly( viewData ) )
    {
        selectedDCS = mcadHandler->GetAssembly( viewData );
    }
    else if( mcadHandler->GetPart( viewData ) )
    {
        selectedDCS = mcadHandler->GetPart( viewData )->GetDCS();
    }
    else
    {
        return;
    }
    
    if( selectMethod == "Glow" )
    {
        if( vxs::SceneManager::instance()->IsRTTOn() )
        {
            selectedDCS->SetTechnique( "Glow" );
        }
        else
        {
            selectedDCS->SetTechnique("Select");
        }
    }

    vx::DeviceHandler::instance()->SetSelectedDCS( selectedDCS.get() );
    osg::BoundingSphere sbs = selectedDCS->getBound();
    
    //Calculate the offset distance
    double distance = 2 * sbs.radius();
    
    ///Get the location of the selected model in local coordinates
    ///This value is always the same no matter where we are
    gmtl::Point3d osgTransformedPosition;
    gmtl::Point3d osgOrigPosition;
    osgTransformedPosition[ 0 ] = sbs.center( ).x( );
    osgTransformedPosition[ 1 ] = sbs.center( ).y( ) - distance;
    osgTransformedPosition[ 2 ] = sbs.center( ).z( );
    osgOrigPosition[ 0 ] = sbs.center( ).x( );
    osgOrigPosition[ 1 ] = sbs.center( ).y( );
    osgOrigPosition[ 2 ] = sbs.center( ).z( );
    
    //Move the center point to the center of the selected object
    osg::ref_ptr< vxs::CoordinateSystemTransform > cst =
    new vxs::CoordinateSystemTransform(
       vxs::SceneManager::instance()->GetActiveSwitchNode(),
       selectedDCS.get(), true );

    gmtl::Matrix44d localToWorldMatrix = cst->GetTransformationMatrix( false );
    
    gmtl::Point3d tempTransPoint = 
    gmtl::makeTrans< gmtl::Point3d >( localToWorldMatrix );
    
    ///Remove the rotation from the transform matrix
    gmtl::Matrix44d tempTrans;
    tempTrans = gmtl::makeTrans< gmtl::Matrix44d >( tempTransPoint );
    double tempRotRad2 = 0.0;
    gmtl::AxisAngled axisAngle( tempRotRad2, 1, 0, 0 );
    gmtl::Quatd quatAxisAngle = gmtl::make< gmtl::Quatd >( axisAngle );
    gmtl::Matrix44d tempRot;
    gmtl::setRot( tempRot, quatAxisAngle );
    gmtl::Matrix44d combineMat = tempTrans;// * tempRot;
    ///Add our end rotation back into the mix
    
    osgTransformedPosition = combineMat * osgTransformedPosition;
    osgOrigPosition = combineMat * osgOrigPosition;
    ///Since the math implies we are doing a delta translation
    ///we need to go grab where we previously were
    double* temp = vxs::SceneManager::instance()->
        GetWorldDCS()->GetVETranslationArray();
    ///Add our distance and previous position back in and get our new end point
    gmtl::Vec3d pos;
    pos[ 0 ] = - osgOrigPosition[ 0 ] + temp[ 0 ];
    pos[ 1 ] = - ( osgOrigPosition[ 1 ] - distance ) + temp[ 1 ];
    pos[ 2 ] = - ( osgOrigPosition[ 2 ] ) + temp[ 2 ];
    
    ///Hand the node we are interested in off to the animation engine
    vx::NavigationAnimationEngine::instance()->SetDCS(
        vxs::SceneManager::instance()->GetWorldDCS() );
    
    ///Hand our created end points off to the animation engine
    vx::NavigationAnimationEngine::instance()->SetAnimationEndPoints(
        pos, quatAxisAngle, true, selectedDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
