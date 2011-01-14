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
#include <ves/xplorer/behavior/Selection.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/Model.h>

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
Selection::Selection()
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
    m_pickConstraint( 0 ),
    m_mouseInsideUI( false )
{    
    CONNECTSIGNALS_4( "%ButtonRelease1%", void( gadget::Keys, int, int, int ), &Selection::ProcessSelection,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_4( "%ButtonPress1%", void( gadget::Keys, int, int, int ), &Selection::RegisterButtonPress,
                     m_connections, any_SignalType, normal_Priority );
    
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ObjectPickedSignal_type >( &m_objectPickedSignal ),
        "KeyboardMouse.ObjectPickedSignal" );
        
    CONNECTSIGNAL_1( "UIManager.EnterLeaveUI", void( bool ), &Selection::UIEnterLeave,
                    m_connections, highest_Priority );
}
////////////////////////////////////////////////////////////////////////////////
Selection::~Selection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Selection::UIEnterLeave( bool insideUI )
{
    m_mouseInsideUI = insideUI;
}
////////////////////////////////////////////////////////////////////////////////
void Selection::RegisterButtonPress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    m_currX = xPos;
    m_currY = yPos;
}
////////////////////////////////////////////////////////////////////////////////
void Selection::ProcessSelection( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    if( m_mouseInsideUI )
    {
        return;
    }

    if( (xPos > m_currX + 2) || (xPos < m_currX - 2) )
    {
        return;
    }

    if( (yPos > m_currY + 2) || (yPos < m_currY - 2) )
    {
        return;
    }

    m_currX = xPos;
    m_currY = yPos;

    //Do not require mod key depending on what the user did
    ClearPointConstraint();
    
    if( m_characterController.IsEnabled() )
    {
        m_characterController.SetCameraRotationSLERP( true );
    }
    
    if( m_cameraManager.IsEnabled() )
    {
        UpdateSelectionLine();
        //If we found a camera
        if( m_cameraManager.Handle(
                                   scenegraph::camera::Event::RELEASE,
                                   *m_lineSegmentIntersector.get() ) )
        {
            ProcessSelection();
            return;
        }
        
        scenegraph::highlight::HighlightManager& highlightManager =
        m_sceneManager.GetHighlightManager();
        if( !highlightManager.IsToggled() )
        {
            ProcessSelection();
            return;
        }
        
        osgUtil::LineSegmentIntersector::Intersections& intersections =
        scenegraph::TestForIntersections(
                                         *m_lineSegmentIntersector.get(),
                                         *m_sceneManager.GetModelRoot() );
        
        //If we found a low level node
        if( intersections.empty() )
        {
            ProcessSelection();
            return;
        }
        
        osg::NodePath nodePath = intersections.begin()->nodePath;
        osg::Node* node = nodePath[ nodePath.size() - 1 ];
        
        if( !node )
        {
            ProcessSelection();
            return;
        }
        
        highlightManager.CreateHighlightCircle( node, nodePath );
        
        ProcessSelection();
        return;
    }
    
    if( m_manipulatorManager.IsEnabled() )
    {
        if( m_manipulatorManager.Handle(
                                        scenegraph::manipulator::Event::RELEASE ) )
        {
            UpdateSelectionLine();
            ProcessSelection();
            return;
        }
    }
    
    //No modifier key
    if( buttonState & gadget::KEY_NONE )
    {
        ;
    }
    //Mod key shift
    else if( buttonState & gadget::KEY_SHIFT )
    {
        ;
    }
    else if( buttonState & gadget::KEY_ALT )
    {
        //OnMouseRelease();
        scenegraph::DCS* infoDCS = DeviceHandler::instance()->GetSelectedDCS();
        DeviceHandler::instance()->UnselectObjects();
        
        std::map< std::string,
        ves::xplorer::plugin::PluginBase* >* tempPlugins =
        ves::xplorer::network::GraphicalPluginManager::instance()->
        GetTheCurrentPlugins();
        
        std::map< std::string, 
        ves::xplorer::plugin::PluginBase* >::iterator pluginIter;
        
        for( pluginIter = tempPlugins->begin(); 
            pluginIter != tempPlugins->end(); ++pluginIter )
        {
            pluginIter->second->GetCFDModel()->
            RenderTextualDisplay( false );
        }
        
        if( !infoDCS )
        {
            UpdateSelectionLine();
            ProcessSelection();
            return;
        }
        osg::Node::DescriptionList descriptorsList;
        descriptorsList = infoDCS->getDescriptions();
        std::string modelIdStr;
        for( size_t i = 0; i < descriptorsList.size(); ++i )
        {
            //std::cout << descriptorsList.at( i ) << std::endl;
            if( descriptorsList.at( i ) == "VE_XML_MODEL_ID" )
            {
                modelIdStr = descriptorsList.at( i + 1 );
                UpdateSelectionLine();
                ProcessSelection();
                return;
            }
        }
        
        pluginIter = tempPlugins->find( modelIdStr );
        if( pluginIter != tempPlugins->end() )
        {
            pluginIter->second->GetCFDModel()->RenderTextualDisplay( true );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Selection::UpdateSelectionLine()
{
    //std::cout << "update selection line " << std::endl;
    osg::Vec3d startPoint, endPoint;
    SetStartEndPoint( startPoint, endPoint );
    m_lineSegmentIntersector->reset();
    m_lineSegmentIntersector->setStart( startPoint );
    m_lineSegmentIntersector->setEnd( endPoint );
    
    //Used to debug the selection line
    //If working correctly, the line should show up as 1 red pixel where picked
    //DrawLine( startPoint, endPoint );
}
////////////////////////////////////////////////////////////////////////////////
void Selection::ProcessSelection()
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
    //mCenterPoint->set( center.x(), center.y(), center.z() );
    
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
void Selection::ClearPointConstraint()
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
void Selection::SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint )
{
    ///In quad buffered stereo this call returns a VPW matrix from a centered
    ///view rather than from one of the eye positions.
    osg::Matrixd inverseVPW( m_sceneManager.GetCurrentGLTransformInfo()->GetVPWMatrixOSG() );
    inverseVPW.invert( inverseVPW );
    startPoint = osg::Vec3d( m_currX, m_currY, 0.0 ) * inverseVPW;
    endPoint = osg::Vec3d( m_currX, m_currY, 1.0 ) * inverseVPW;
    
    //std::cout << m_currX << " " << m_currY << std::endl;
    //std::cout << "startPoint: " << startPoint << std::endl;
    //std::cout << "endPoint: " << endPoint << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
