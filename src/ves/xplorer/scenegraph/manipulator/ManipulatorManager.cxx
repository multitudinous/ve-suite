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
#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>
#include <ves/xplorer/scenegraph/manipulator/RotateCompound.h>
#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>
#include <ves/xplorer/scenegraph/manipulator/Rotate.h>
#include <ves/xplorer/scenegraph/manipulator/HelpCircle.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/Depth>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ManipulatorManager::ManipulatorManager()
    :
    osg::Group(),
    m_enabled( false ),
    m_leafDraggerIsActive( false ),
    m_rootDragger( NULL ),
    m_leafDragger( NULL ),
    m_sceneManipulator( NULL ),
    m_draggerSize( 64.0 )
{
    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();
    stateSet->setRenderBinDetails( 11, std::string( "DepthSortedBin" ) );

    setCullingActive( false );

    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    depth->setFunction( osg::Depth::ALWAYS );
    depth->setWriteMask( false );
    stateSet->setAttributeAndModes( 
        depth.get(), 
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    Enable();

    //Create rotate twist dragger
    m_rotateTwist = new RotateTwist();
    m_rotateTwist->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 1.0, 1.0, 1.0 ), true );
    m_rotateTwist->Hide();
    //addChild( m_rotateTwist.get() );

    m_sceneManipulator = new TransformManipulator();
    //Turn off the scene manipulator until requested by user
    m_sceneManipulator->Hide();
    addChild( m_sceneManipulator.get() );
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorManager::ManipulatorManager(
    const ManipulatorManager& manipulatorManager, const osg::CopyOp& copyop )
    :
    osg::Group( manipulatorManager, copyop ),
    m_enabled( manipulatorManager.m_enabled ),
    m_nodePath( manipulatorManager.m_nodePath ),
    m_nodePathItr( manipulatorManager.m_nodePathItr ),
    m_rootDragger( manipulatorManager.m_rootDragger ),
    m_leafDragger( manipulatorManager.m_leafDragger )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorManager::~ManipulatorManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::addChild( Dragger* child )
{
    //If rotation-type dragger, create a help circle
    if( child->GetTransformationType() & TransformationType::ROTATE_COMPOUND )
    {
        //Insert help circle to front so it gets traversed first
        osg::ref_ptr< HelpCircle > helpCircle = new HelpCircle();
        helpCircle->SetRootDragger( child );

        CompoundDragger* compoundDragger = child->AsCompoundDragger();
        if( compoundDragger )
        {
            compoundDragger->SetHelpCircle( helpCircle.get() );
            RotateCompound* rotateCompound =
                compoundDragger->AsRotateCompound();
            if( rotateCompound )
            {
                rotateCompound->insertChild( 0, helpCircle.get() );
            }

            //Reset geometry
            compoundDragger->SetEnabledModes(
                compoundDragger->GetEnabledModes() );
        }
        else
        {
            Rotate* rotate = child->AsRotate();
            if( rotate )
            {
                rotate->SetHelpCircle( helpCircle.get() );
                child->insertChild( 0, helpCircle.get() );
            }
        }
    }

    //Initialize root dragger
    child->SetScale( m_draggerSize );
    if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        //If desktop mode
        child->SetAutoScaleToScreen( true );
    }
    else
    {
        //If cave mode
        child->SetAutoScaleToScreen( false );
    }

    return osg::Group::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
/*
osg::BoundingSphere ManipulatorManager::computeBound() const
{
    osg::BoundingSphere bsphere;

    return bsphere;
}
*/
////////////////////////////////////////////////////////////////////////////////
void ManipulatorManager::Enable( const bool& enable )
{
    m_enabled = enable;

    if( m_enabled )
    {
        setNodeMask( NodeMask::MANIPULATOR_MANAGER );
    }
    else
    {
        setNodeMask( NodeMask::NONE );
    }
}
////////////////////////////////////////////////////////////////////////////////
Dragger* ManipulatorManager::ConvertNodeToDragger( osg::Node* node )
{
    return dynamic_cast< Dragger* >( node );
}
////////////////////////////////////////////////////////////////////////////////
Dragger* ManipulatorManager::GetChild( unsigned int i )
{
    return dynamic_cast< Dragger* >( osg::Group::getChild( i ) );
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist* const ManipulatorManager::GetTwistManipulator() const
{
    return m_rotateTwist.get();
}
////////////////////////////////////////////////////////////////////////////////
TransformManipulator* const ManipulatorManager::GetSceneManipulator() const
{
    return m_sceneManipulator.get();
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::Handle(
    Event::Enum event,
    osgUtil::LineSegmentIntersector* lineSegmentIntersector )
{
    //If we want to test for dragger intersections
    //For the release and drag events we do not need to test for intersections
    if( lineSegmentIntersector )
    {
        if( !TestForIntersections( lineSegmentIntersector ) )
        {
            vprDEBUG( vesDBG, 3 ) << "|\tNo manipulator intersections, event = " 
                << event << std::endl << vprDEBUG_FLUSH;
            return false;
        }
    }

    //Make sure the root drager is valid
    if( !m_rootDragger )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tNo root dragger." 
            << std::endl << vprDEBUG_FLUSH;
        return false;
    }

    switch( event )
    {
    case Event::FOCUS:
    {
        m_leafDragger = m_rootDragger->Focus( m_nodePathItr );
        return m_leafDragger;
    }
    case Event::PUSH:
    {
        //Protect against multiple pushes from multiple devices
        bool returnTemp( false );
        if( m_leafDraggerIsActive )
        {
            returnTemp = Handle( Event::RELEASE );
        }

        m_leafDragger =
            m_rootDragger->Push( *m_deviceInput, m_nodePath, m_nodePathItr );
        if( !m_leafDragger )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tFailed to activate a dragger." 
                << std::endl << vprDEBUG_FLUSH;
        }
        m_leafDraggerIsActive = true;
        ///What does this logic mean???
        return m_leafDragger && returnTemp;
    }
    case Event::DRAG:
    {
        if( !m_leafDragger )
        {
            return false;
        }

        m_leafDragger->Drag( *m_deviceInput );
        return m_leafDragger;
    }
    case Event::RELEASE:
    {
        m_leafDragger = NULL;
        m_leafDraggerIsActive = false;
        return m_rootDragger->Release( m_nodePathItr );
    }
    default:
    {
        m_leafDragger = NULL;

        return false;
    }
    } //end switch( event )
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::insertChild( unsigned int index, Dragger* child )
{
    return osg::Group::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
bool const& ManipulatorManager::IsEnabled() const
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
bool const& ManipulatorManager::LeafDraggerIsActive() const
{
    return m_leafDraggerIsActive;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::replaceChild( Dragger* origChild, Dragger* newChild )
{
    return osg::Group::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::setChild( unsigned int i, Dragger* node )
{
    return osg::Group::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::TestForIntersections(
    osgUtil::LineSegmentIntersector* lineSegmentIntersector )
{
    osgUtil::IntersectionVisitor intersectionVisitor( lineSegmentIntersector );
    accept( intersectionVisitor );

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        lineSegmentIntersector->getIntersections();
    if( intersections.empty() )
    {
        //Reset the root dragger
        if( m_rootDragger )
        {
            m_rootDragger = NULL;
        }

        //Reset the leaf dragger
        if( m_leafDragger )
        {
            m_leafDragger->UseColor( Color::DEFAULT );
            m_leafDragger = NULL;
        }

        return false;
    }

    m_deviceInput = lineSegmentIntersector;

    //Get the full node path from selected dragger to this
    m_nodePath = intersections.begin()->nodePath;
    m_nodePathItr = m_nodePath.begin();
    //Increment past this
    ++m_nodePathItr;
    m_rootDragger = ConvertNodeToDragger( *m_nodePathItr );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorManager::SetDraggerScale( double draggerScale )
{
    m_draggerSize = draggerScale;
}
////////////////////////////////////////////////////////////////////////////////
