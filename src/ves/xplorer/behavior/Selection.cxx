/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <switchwire/BooleanPropagationCombiner.h>

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
#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/model/Model.h>

#include <boost/concept_check.hpp>

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

//osgWorks
#include <osgwTools/NodePathUtils.h>

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
    m_cadSelectionMode( false )
{
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease1%", bool( gadget::Keys, int, int, int ),
                               switchwire::BooleanPropagationCombiner, &Selection::ProcessSelection,
                               m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress1%", bool( gadget::Keys, int, int, int ),
                               switchwire::BooleanPropagationCombiner, &Selection::RegisterButtonPress,
                               m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_2( "KeyboardMouse.StartEndPoint", void( osg::Vec3d, osg::Vec3d ), &Selection::SetStartEndPoint,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_3_COMBINER( "KeyboardMouse.KeyPress_KEY_Z",
                               bool( gadget::Keys, int, char ),
                               switchwire::BooleanPropagationCombiner,
                               &Selection::ProcessUndoEvent, m_connections,
                               keyboard_SignalType, normal_Priority );

    CONNECTSIGNALS_1( "%CADSelection",
                      void( bool const & flag ),
                      &Selection::SetCADSelection,
                      m_connections, any_SignalType, normal_Priority );

    ///Handle some wand signals
    CONNECTSIGNALS_4( "Wand.ButtonRelease0%", void( gadget::Keys, int, int, int ), &Selection::ProcessSelection,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_2( "Wand.StartEndPoint", void( osg::Vec3d, osg::Vec3d ), &Selection::SetStartEndPoint,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_1( "%HighlightAndSetManipulators", void( osg::NodePath& ),
                      &Selection::HighlightAndSetManipulators,
                      m_connections, any_SignalType, high_Priority );

    CONNECTSIGNALS_1( "%HighlightNode", void( osg::NodePath& ),
                      &Selection::HighlightNode,
                      m_connections, any_SignalType, high_Priority );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_objectPickedSignal ),
        "Selection.ObjectPickedSignal" );

    CONNECTSIGNALS_1( "Selection.ObjectPickedSignal", void( osg::NodePath& ),
                      &Selection::ConvertNodePathToString,
                      m_connections, any_SignalType, normal_Priority );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_objectPickedAsStringSignal ),
        "Selection.ObjectPickedSignalAsString" );
}
////////////////////////////////////////////////////////////////////////////////
Selection::~Selection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Selection::RegisterButtonPress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    boost::ignore_unused_variable_warning( buttonKey );
    boost::ignore_unused_variable_warning( buttonState );

    m_currX = xPos;
    m_currY = yPos;

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool Selection::ProcessUndoEvent( gadget::Keys keyPress, int modifierState, char keyChar )
{
    boost::ignore_unused_variable_warning( keyPress );
    boost::ignore_unused_variable_warning( keyChar );

    if( !m_cadSelectionMode )
    {
        return false;
    }

    if( ( modifierState & gadget::CTRL_MASK ) ||
            ( modifierState & gadget::KEY_COMMAND ) )
    {
        if( m_unselectedCADFiles.size() )
        {
            m_unselectedCADFiles.back()->setNodeMask( 1 );
            m_unselectedCADFiles.pop_back();
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool Selection::ProcessSelection( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    boost::ignore_unused_variable_warning( buttonKey );

    if( ( xPos > m_currX + 2 ) || ( xPos < m_currX - 2 ) )
    {
        return false;
    }

    if( ( yPos > m_currY + 2 ) || ( yPos < m_currY - 2 ) )
    {
        return false;
    }

    m_currX = xPos;
    m_currY = yPos;

    //No modifier key
    if( buttonState & gadget::KEY_NONE )
    {
        //return false;
    }
    //Mod key shift
    else if( buttonState & gadget::SHIFT_MASK )
    {
        //return false;
    }
    else if( buttonState & gadget::ALT_MASK )
    {
        //OnMouseRelease();
        scenegraph::DCS* infoDCS = DeviceHandler::instance()->GetSelectedDCS();
        DeviceHandler::instance()->UnselectObjects();

        std::map < std::string,
            ves::xplorer::plugin::PluginBasePtr > * tempPlugins =
                ves::xplorer::network::GraphicalPluginManager::instance()->
                GetTheCurrentPlugins();

        std::map < std::string,
            ves::xplorer::plugin::PluginBasePtr >::iterator pluginIter;

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
            return false;
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
                return false;
            }
        }

        pluginIter = tempPlugins->find( modelIdStr );
        if( pluginIter != tempPlugins->end() )
        {
            pluginIter->second->GetCFDModel()->RenderTextualDisplay( true );
        }
        //return false;
    }

    UpdateSelectionLine();
    ProcessSelection();
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void Selection::UpdateSelectionLine()
{
    m_lineSegmentIntersector->reset();
    m_lineSegmentIntersector->setStart( m_startPoint );
    m_lineSegmentIntersector->setEnd( m_endPoint );
}
////////////////////////////////////////////////////////////////////////////////
void Selection::ProcessSelection()
{
    //Unselect the previous selected DCS
    DeviceHandler::instance()->UnselectObjects();

    //Remove custom glows
    {
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor
        highlightRemove(
            ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot(),
            "", false, true );
    }

    //Test for intersections
    osgUtil::LineSegmentIntersector::Intersections& intersections =
        scenegraph::TestForIntersections(
            *m_lineSegmentIntersector.get(),
            *m_sceneManager.GetModelRoot() );
    if( intersections.empty() )
    {
        vprDEBUG( vesDBG, 1 )
                << "|\tSelection::ProcessSelection No object selected"
                << std::endl << vprDEBUG_FLUSH;

        // Tell everyone else we have a null selection. nullPath must be l-value
        // since signal passes it by reference
        osg::NodePath nullPath;
        m_objectPickedSignal.signal( nullPath );

        return;
    }

    //Search for first item that is not the laser
    {
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
            vprDEBUG( vesDBG, 1 ) << "|\tSelection::ProcessSelection No object selected"
                                  << std::endl << vprDEBUG_FLUSH;

            ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
                ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );

            return;
        }
        ///Print the name of the first node hit
        osg::NodePath nodePath = intersections.begin()->nodePath;
        osg::Node* node = nodePath[ nodePath.size() - 1 ];
        vprDEBUG( vesDBG, 1 )
                << "|\tSelection::ProcessSelection The name of the first node hit is "
                << node->getName() << std::endl << vprDEBUG_FLUSH;
    }

    ///CAD selection code
    if( m_cadSelectionMode )
    {
        osg::NodePath nodePath = intersections.begin()->nodePath;
        osg::Node* node = nodePath[ nodePath.size() - 1 ];
        node->setNodeMask( 0 );
        m_unselectedCADFiles.push_back( node );
        return;
    }

    //Now find the new selected object
    osg::NodePath nodePath = intersections.begin()->nodePath;
    m_objectPickedSignal.signal( nodePath );
    osg::Node* vesObject = scenegraph::FindVESObject( nodePath );
    boost::ignore_unused_variable_warning( vesObject );

    //HighlightAndSetManipulators alters nodePath, so copy it off first.
    osg::NodePath nodePathCopy( nodePath );
    HighlightAndSetManipulators( nodePath );

    ///Send the data back to the ui for the expanding tree
    //m_objectPickedSignal.signal( nodePathCopy );
}
////////////////////////////////////////////////////////////////////////////////
void Selection::HighlightAndSetManipulators( osg::NodePath& nodePath )
{
    osg::Node* vesObject = scenegraph::FindVESObject( nodePath );
    if( !vesObject )
    {
        vprDEBUG( vesDBG, 1 )
                << "|\tSelection::HighlightAndSetManipulators Invalid object selected"
                << std::endl << vprDEBUG_FLUSH;

        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
            ves::xplorer::scenegraph::SceneManager::instance()->GetNavDCS() );
        return;
    }

    //Right now we are saying you must have a DCS
    scenegraph::DCS* newSelectedDCS = dynamic_cast< scenegraph::DCS* >( vesObject );
    if( !newSelectedDCS )
    {
        return;
    }

    //Remove custom glows
    ves::xplorer::scenegraph::HighlightNodeByNameVisitor
    highlight2( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot(), "", false, true );

    //
    osg::Vec3d center( 0.0, 0.0, 0.0 );
    //If dcs is from a camera object, we want to rotate about local zero point
    //We really need a selection callback for selectable objects -_-
    if( newSelectedDCS->getName() != "CameraDCS" )
    {
        center = vesObject->getBound().center();

        //Remove local node from nodePath
        nodePath.pop_back();
    }
    else
    {
        ;
    }

    osg::Matrixd localToWorldMatrix = osg::computeLocalToWorld( nodePath );
    center = center * localToWorldMatrix;


    if( m_manipulatorManager.IsEnabled() )
    {
        //Set the connection between the scene manipulator and the selected dcs
        scenegraph::manipulator::TransformManipulator* sceneManipulator =
            m_manipulatorManager.GetSceneManipulator();
        sceneManipulator->Connect( newSelectedDCS );
        sceneManipulator->SetPosition( center );
    }

    //In the future the center point will be in world coordinates
    //m_sceneManager.GetCenterPoint().set( center.x(), center.y(), center.z() );
    ves::xplorer::scenegraph::SceneManager::instance()->GetMxCoreViewMatrix().setOrbitCenterPoint( osg::Vec3d( center.x(), center.y(), center.z() ) );
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



    vprDEBUG( vesDBG, 1 ) << "|\tObject has name "
                          << vesObject->getName()
                          << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 ) << "|\tObject's descriptors "
                          << vesObject->getDescriptions().at( 1 )
                          << std::endl << vprDEBUG_FLUSH;

    // Change the active model to the one corresponding to this piece of geometry
    ves::open::xml::model::ModelPtr model = newSelectedDCS->GetModelData();
    if( model )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tObject is part of model "
                              << newSelectedDCS->GetModelData()->GetID()
                              << std::endl << vprDEBUG_FLUSH;
        ves::xplorer::ModelHandler::instance()->
            SetActiveModel( newSelectedDCS->GetModelData()->GetID() );
    }
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
void Selection::SetStartEndPoint( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    m_startPoint = startPoint;
    m_endPoint = endPoint;
}
////////////////////////////////////////////////////////////////////////////////
void Selection::SetCADSelection( bool const& flags )
{
    m_cadSelectionMode = flags;
}
////////////////////////////////////////////////////////////////////////////////
void Selection::HighlightNode( osg::NodePath& nodePath )
{
    if( nodePath == osg::NodePath() )
    {
        return;
    }

    if( !m_sceneManager.IsRTTOn() )
    {
        return;
    }

    //Remove custom glows
    ves::xplorer::scenegraph::HighlightNodeByNameVisitor
    highlight2( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot(), "", false, true );

    osg::Node* node = nodePath.at( nodePath.size() - 1 );
    vprDEBUG( vesDBG, 1 )
            << "|\tSelection::HighlightNode The name of the first node hit is "
            << node->getName() << std::endl << vprDEBUG_FLUSH;
    osg::ref_ptr< osg::StateSet > geode_stateset = node->getOrCreateStateSet();
    //I think we need to check and see if the stateset has any parents
    if( geode_stateset->getNumParents() > 1 )
    {
        //std::cout << drawable_stateset->getNumParents() << std::endl;
        //std::cout << "StateSet is shared." << std::endl;
        osg::ref_ptr< osg::StateSet > temp_stateset =
            new osg::StateSet( *( geode_stateset.get() ), osg::CopyOp::DEEP_COPY_ALL );
        node->setStateSet( temp_stateset.get() );
    }

    //Add shader code to have code highlighted
    osg::Vec3 enableGlow( 1.0, 0.0, 0.0 );
    geode_stateset->addUniform( new osg::Uniform( "glowColor", enableGlow ) );
}
////////////////////////////////////////////////////////////////////////////////
void Selection::ConvertNodePathToString( osg::NodePath& nodePath )
{
    m_objectPickedAsStringSignal.signal( osgwTools::nodePathToString( nodePath ) );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
