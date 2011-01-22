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
#include <ves/xplorer/behavior/WandEvents.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/xplorer/device/Wand.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/environment/cfdDisplaySettings.h>

#include <ves/xplorer/eventmanager/EventManager.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>
//#include <ves/xplorer/scenegraph/SetStateOnNURBSNodeVisitor.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>
#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/model/Model.h>

//OSG
#include <osg/BoundingSphere>
#include <osg/Vec3d>
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/AutoTransform>
#include <osg/io_utils>

//GMTL
#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

namespace ves
{
namespace xplorer
{
namespace behavior
{
////////////////////////////////////////////////////////////////////////////////
WandEvents::WandEvents()
    :
    m_physicsSimulator( *ves::xplorer::scenegraph::PhysicsSimulator::instance() ),
    m_sceneManager( *ves::xplorer::scenegraph::SceneManager::instance() ),
    m_characterController( m_sceneManager.GetCharacterController() ),
    m_manipulatorManager( m_sceneManager.GetManipulatorManager() ),
    m_cameraManager( m_sceneManager.GetCameraManager() ),
    m_lineSegmentIntersector( new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) ),
    m_currX( 0 ),
    m_currY( 0 ),
    m_pickedBody( 0 ),
    m_pickConstraint( 0 )
{    
    CONNECTSIGNALS_4( "Wand.ButtonRelease0%", void( gadget::Keys, int, int, int ), &WandEvents::Button0ReleaseEvent,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_4( "Wand.ButtonPress0%", void( gadget::Keys, int, int, int ), &WandEvents::Button0PressEvent,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_4( "Wand.ButtonOn0%", void( gadget::Keys, int, int, int ), &WandEvents::Button0PressEvent,
                     m_connections, any_SignalType, normal_Priority );
    
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ObjectPickedSignal_type >( &m_objectPickedSignal ),
        "WandEvents.ObjectPickedSignal" );
        
    m_cadSelectionMode = false;
}
////////////////////////////////////////////////////////////////////////////////
WandEvents::~WandEvents()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void WandEvents::Button0OnEvent( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    //UpdateSelectionLine( true );
    
    ///Push the FOCUS event if we are using manipulators and a dragger is not
    ///active
    if( m_manipulatorManager.IsEnabled() && 
       !m_manipulatorManager.LeafDraggerIsActive() )
    {
        //osg::ref_ptr< osg::StateSet > stateset = 
        //m_wand->GetWandTransform().getOrCreateStateSet();
        if( m_manipulatorManager.Handle( scenegraph::manipulator::Event::FOCUS,
                                        m_lineSegmentIntersector.get() ) )
        {
            
            //stateset->setRenderBinDetails( 11, std::string( "RenderBin" ) );                
            //stateset->setAttributeAndModes( m_depth.get(), 
            //                               osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
            //m_wand->GetWandTransform().setCullingActive( false );
        }
        else
        {
            //stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
            //stateset->removeAttribute( m_depth.get() );
            //m_wand->GetWandTransform().setCullingActive( true );
        }
    }
    
    if( m_manipulatorManager.IsEnabled() && 
       m_manipulatorManager.LeafDraggerIsActive() )
    {
        if( m_manipulatorManager.Handle(
                                        scenegraph::manipulator::Event::DRAG ) )
        {
            return;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void WandEvents::Button0PressEvent( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    m_currX = xPos;
    m_currY = yPos;
}
////////////////////////////////////////////////////////////////////////////////
void WandEvents::Button0ReleaseEvent( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    //std::cout << " Button0ReleaseEvent 2 " << std::endl;
    //UpdateSelectionLine( false );
    
    if( m_manipulatorManager.IsEnabled() && 
       m_manipulatorManager.LeafDraggerIsActive() )
    {
        bool success = m_manipulatorManager.Handle( 
                                                   scenegraph::manipulator::Event::RELEASE );
        if( success )
        {
            vprDEBUG( vesDBG, 2 ) << "|\tSuccessfully released a dragger." 
            << std::endl << vprDEBUG_FLUSH;
            return;
        }
    }
    
    if( m_manipulatorManager.IsEnabled() )
    {
        vprDEBUG( vesDBG, 2 ) << "|\tTrying to push a dragger." 
        << std::endl << vprDEBUG_FLUSH;
        bool success = m_manipulatorManager.Handle(
                                                   scenegraph::manipulator::Event::PUSH,
                                                   m_lineSegmentIntersector.get() );
        if( m_manipulatorManager.LeafDraggerIsActive() )
        {
            vprDEBUG( vesDBG, 2 ) << "|\tSuccessfully pushed a dragger." 
            << std::endl << vprDEBUG_FLUSH;
            return;
        }
    }
    
    //ProcessHit();
}
////////////////////////////////////////////////////////////////////////////////
void WandEvents::UpdateSelectionLine()
{
    //std::cout << "update selection line " << std::endl;
    //osg::Vec3d startPoint, endPoint;
    //SetStartEndPoint( startPoint, endPoint );
    m_lineSegmentIntersector->reset();
    //m_lineSegmentIntersector->setStart( m_startPoint );
    //m_lineSegmentIntersector->setEnd( m_endPoint );
    
    //Used to debug the selection line
    //If working correctly, the line should show up as 1 red pixel where picked
    //DrawLine( startPoint, endPoint );
}
////////////////////////////////////////////////////////////////////////////////
void WandEvents::ProcessSelection()
{
    //Unselect the previous selected DCS
    DeviceHandler::instance()->UnselectObjects();
    
    //Test for intersections
    osgUtil::LineSegmentIntersector::Intersections& intersections =
    scenegraph::TestForIntersections(
                                     *m_lineSegmentIntersector.get(), *m_sceneManager.GetModelRoot() );
    if( intersections.empty() )
    {
        vprDEBUG( vesDBG, 1 )
        << "|\tKeyboardMouse::ProcessHit No object selected"
        << std::endl << vprDEBUG_FLUSH;
        
        return;
    }
    
    //Now find the new selected object
    osg::NodePath nodePath = intersections.begin()->nodePath;
    osg::Node* vesObject = scenegraph::FindVESObject( nodePath );
    if( !vesObject )
    {
        vprDEBUG( vesDBG, 1 )
        << "|\tKeyboardMouse::ProcessHit Invalid object selected"
        << std::endl << vprDEBUG_FLUSH;
        
        return;
    }
    
    //Right now we are saying you must have a DCS
    scenegraph::DCS* newSelectedDCS = dynamic_cast< scenegraph::DCS* >( vesObject );
    if( !newSelectedDCS )
    {
        return;
    }
    
    //Set the connection between the scene manipulator and the selected dcs
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
    m_manipulatorManager.GetSceneManipulator();
    sceneManipulator->Connect( newSelectedDCS );
    
    //
    osg::Vec3d center( 0.0, 0.0, 0.0 );
    //If dcs is from a camera object, we want to rotate about local zero point
    //We really need a selection callback for selectable objects -_-
    if( newSelectedDCS->getName() != "CameraDCS" )
    {
        center= vesObject->getBound().center();
        
        //Remove local node from nodePath
        nodePath.pop_back();
    }
    else
    {
        ;
    }
    osg::Matrixd localToWorldMatrix = osg::computeLocalToWorld( nodePath );
    center = center * localToWorldMatrix;
    sceneManipulator->SetPosition( center );
    
    //We need to transform center point into camera space
    //In the future the center point will be in world coordinates
    center = center * osg::Matrixd( m_sceneManager.GetNavDCS()->GetMat().mData );
    //center = center * m_currentGLTransformInfo->GetViewMatrixOSG();
    m_sceneManager.GetCenterPoint().set( center.x(), center.y(), center.z() );
    
    //Set the selected DCS
    DeviceHandler::instance()->SetSelectedDCS( newSelectedDCS );
    
    //Need to do this for multi-pass techniques
    if( m_sceneManager.IsRTTOn() )
    {
        newSelectedDCS->SetTechnique( "Glow" );
    }
    else
    {
        newSelectedDCS->SetTechnique( "Select" );
    }
    
    vprDEBUG( vesDBG, 1 ) << "|\tObjects has name "
        << vesObject->getName()
        << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 ) << "|\tObjects descriptors "
        << vesObject->getDescriptions().at( 1 )
        << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 ) << "|\tObject is part of model "
        << newSelectedDCS->GetModelData()->GetID()
        << std::endl << vprDEBUG_FLUSH;
    
    // Change the active model to the one corresponding to this piece of geometry
    ves::xplorer::ModelHandler::instance()->
        SetActiveModel( newSelectedDCS->GetModelData()->GetID() );
    
    // Can't just pass in local variable nodePath because it is altered in call to 
    // FindVESObject
    osg::NodePath np = intersections.begin()->nodePath;
    ///Send the data back to the ui for the expanding tree
    m_objectPickedSignal( np );
}
////////////////////////////////////////////////////////////////////////////////
void WandEvents::ClearPointConstraint()
{
    //Do not require mod key depending on what the user did
    if( m_pickConstraint )
    {
        m_physicsSimulator.GetDynamicsWorld()->
            removeConstraint( m_pickConstraint );

        delete m_pickConstraint;
        m_pickConstraint = NULL;
        
        m_pickedBody->forceActivationState( ACTIVE_TAG );
        m_pickedBody->setDeactivationTime( 0.0 );
        m_pickedBody = NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
void WandEvents::ProcessHit()
{
    osgUtil::LineSegmentIntersector::Intersections& intersections =
    scenegraph::TestForIntersections( *m_lineSegmentIntersector.get(),
                                     *m_sceneManager.GetModelRoot() );
    
    //Unselect the previous selected DCS
    DeviceHandler::instance()->UnselectObjects();
    
    //Now find the new selected DCS
    if( intersections.empty() )
    {
        vprDEBUG( vesDBG, 1 )
        << "|\tWand::ProcessHit No object selected"
        << std::endl << vprDEBUG_FLUSH;
        
        return;
    }
    
    //Search for first item that is not the laser
    osg::Node* objectHit( NULL );
    for( osgUtil::LineSegmentIntersector::Intersections::iterator itr = 
        intersections.begin(); itr != intersections.end(); ++itr )
    {
        objectHit = *( itr->nodePath.rbegin() );
        if( objectHit->getName() != "Laser" &&
           objectHit->getName() != "Root Node" )
        {
            break;
        }
    }        
    
    if( !objectHit )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tWand::ProcessHit No object selected"
        << std::endl << vprDEBUG_FLUSH;
        
        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
                                                              ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
        
        return;
    }
    
    ves::xplorer::scenegraph::camera::CameraManager& cameraManager = 
    ves::xplorer::scenegraph::SceneManager::instance()->GetCameraManager();
    bool cptEnabled = cameraManager.IsCPTEnabled();
    if( cptEnabled )
    {
        scenegraph::highlight::HighlightManager& highlightManager =
        m_sceneManager.GetHighlightManager();
        if( m_cameraManager.Handle( scenegraph::camera::Event::RELEASE,
                                   *m_lineSegmentIntersector.get() ) )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tWand::ProcessHit Selected a Camera"
            << std::endl << vprDEBUG_FLUSH;
        }
        else if( highlightManager.IsToggled() )
        {            
            //If we found a low level node
            osg::NodePath nodePath = intersections.begin()->nodePath;
            osg::Node* node = nodePath[ nodePath.size() - 1 ];
            
            highlightManager.CreateHighlightCircle( node, nodePath );
            return;
        }
    }    
    
    if( m_cadSelectionMode )
    {
        osg::NodePath nodePath = intersections.begin()->nodePath;
        osg::Node* node = nodePath[ nodePath.size() - 1 ];
        node->setNodeMask( 0 );
        m_unselectedCADFiles.push_back( node );
        return;
    }
    
    //Now find the id for the cad
    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( objectHit );
    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
    if( parentNode.valid() )
    {
        scenegraph::DCS* newSelectedDCS =
        static_cast< scenegraph::DCS* >( parentNode.get() );
        
        vprDEBUG( vesDBG, 1 ) << "|\tObjects has name "
        << parentNode->getName() << std::endl << vprDEBUG_FLUSH;
        
        vprDEBUG( vesDBG, 1 ) << "|\tObjects descriptors "
        << parentNode->getDescriptions().at( 1 )
        << std::endl << vprDEBUG_FLUSH;
        
        if( ves::xplorer::scenegraph::SceneManager::instance()->IsRTTOn() )
        {
            newSelectedDCS->SetTechnique( "Glow" );
        }
        else
        {
            newSelectedDCS->SetTechnique( "Select" );
        }
        DeviceHandler::instance()->SetSelectedDCS( newSelectedDCS );
        
        //Set the connection between the scene manipulator and the selected dcs
        scenegraph::manipulator::TransformManipulator* sceneManipulator =
        m_manipulatorManager.GetSceneManipulator();
        sceneManipulator->Connect( newSelectedDCS );
        
        //Move the scene manipulator to the center point
        scenegraph::LocalToWorldNodePath nodePath(
            newSelectedDCS, m_sceneManager.GetModelRoot() );
        scenegraph::LocalToWorldNodePath::NodeAndPathList npl =
        nodePath.GetLocalToWorldNodePath();
        scenegraph::LocalToWorldNodePath::NodeAndPath nap = npl.at( 0 );
        osg::Matrixd localToWorld = osg::computeLocalToWorld( nap.second );
        osg::Vec3d newCenter = newSelectedDCS->getBound().center() * localToWorld;
        sceneManipulator->SetPosition( newCenter );
        vprDEBUG( vesDBG, 1 ) << "|\tEnd Selection "
        << std::endl << vprDEBUG_FLUSH;
    }
    else
    {
        vprDEBUG( vesDBG, 1 ) << "|\tObject does not have name parent name "
        << objectHit->getParents().front()->getName()
        << std::endl << vprDEBUG_FLUSH;
        
        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
            ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
    }
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
