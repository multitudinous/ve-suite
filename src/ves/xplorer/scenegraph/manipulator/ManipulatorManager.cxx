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
#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/Depth>
#include <osg/AutoTransform>

#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ManipulatorManager::ManipulatorManager()
    :
    osg::Group(),
    m_enabled( false ),
    //NodeMask is an unsigned int
    m_nodeMask( 0xfffffffe ),
    m_rootDragger( NULL ),
    m_leafDragger( NULL ),
    m_sceneManipulator( NULL )
{
    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();
    stateSet->setRenderBinDetails( 11, std::string( "DepthSortedBin" ) );

    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    depth->setFunction( osg::Depth::ALWAYS );
    depth->setWriteMask( false );
    stateSet->setAttributeAndModes( 
        depth.get(), 
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    Enable();

    //double initialScale = 10.0;
    //setScale( initialScale );
    //Set manipulator to scale to the same size based off distance from the eye
    //setAutoScaleToScreen( true );
    //Set initial bound so AutoTransform is not culled by small feature culling
    //osg::BoundingSphere bs( osg::Vec3f( 0.0, 0.0, 0.0 ), initialScale );
    //setInitialBound( bs );

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
    m_nodeMask( manipulatorManager.m_nodeMask ),
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
    //new osg::PositionAttitudeTransform();
    //dragger.SetTransform( transform );

    return osg::Group::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingSphere ManipulatorManager::computeBound() const
{
    osg::BoundingSphere bsphere;

    return bsphere;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::Connect( Dragger& dragger, osg::Transform& transform )
{
    //Check to see if the transform is already associated with the dragger
    if( m_draggerAssociationMap.count( &dragger ) > 0 )
    {
        std::pair< DraggerAssociationMap::iterator,
                   DraggerAssociationMap::iterator > range;
        range = m_draggerAssociationMap.equal_range( &dragger );
        DraggerAssociationMap::iterator itr = range.first;
        for( itr; itr != range.second; ++itr )
        {
            if( itr->second == &transform )
            {
                return false;
            }
        }
    }

    //Associate transform with dragger
    m_draggerAssociationMap.insert( std::make_pair( &dragger, &transform ) );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorManager::Disconnect( Dragger& dragger )
{
    m_draggerAssociationMap.erase( &dragger );
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorManager::Enable( const bool& enable )
{
    m_enabled = enable;

    if( m_enabled )
    {
        setNodeMask( m_nodeMask );
    }
    else
    {
        setNodeMask( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
Dragger* ManipulatorManager::ConvertNodeToDragger( osg::Node* node )
{
    return static_cast< Dragger* >( node );
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorManager::ComputeAssociatedMatrices()
{
    /*
    //Compute local to world and world to local matrices
    //for all associated node's transforms
    std::vector< osg::Transform* >::const_iterator itr =
        m_associatedTransforms.find( m_rootDragger )->second.begin();
    for( itr; itr != m_associatedTransforms.end(); ++itr )
    {
        LocalToWorldNodePath nodePath(
            *itr, SceneManager::instance()->GetModelRoot() );
        LocalToWorldNodePath::NodeAndPathList npl =
            nodePath.GetLocalToWorldNodePath();
        LocalToWorldNodePath::NodeAndPath nap = npl.at( 0 );

        osg::Matrixd localToWorld = osg::computeLocalToWorld( nap.second );
        osg::Matrixd worldToLocal = osg::Matrixd::inverse( localToWorld );
        m_coordinateSystemTransforms[ *itr ] =
            std::make_pair( localToWorld, worldToLocal );
    }

    //Turn off automatic scaling for the dragger
    //osg::AutoTransform* autoTransform =
    //static_cast< osg::AutoTransform* >(
        //m_parentManipulator->getParent( 0 ) );
    //autoTransform->setAutoScaleToScreen( false );
    */
}
////////////////////////////////////////////////////////////////////////////////
Dragger* ManipulatorManager::GetChild( unsigned int i )
{
    return static_cast< Dragger* >( osg::Group::getChild( i ) );
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
    if( lineSegmentIntersector )
    {
        if( !TestForIntersections( lineSegmentIntersector ) )
        {
            return false;
        }
    }

    //Make sure the root drager is valid
    if( !m_rootDragger )
    {
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
            m_leafDragger =
                m_rootDragger->Push( *m_deviceInput, m_nodePath, m_nodePathItr );

            ComputeAssociatedMatrices();

            return m_leafDragger;
        }
        case Event::DRAG:
        {
            if( !m_leafDragger )
            {
                return false;
            }

            m_leafDragger->Drag( *m_deviceInput );

            UpdateAssociatedTransforms();

            return m_leafDragger;
        }
        case Event::RELEASE:
        {
            m_leafDragger = NULL;

            return m_rootDragger->Release( m_nodePathItr );
        }
        default:
        {
            m_leafDragger = NULL;

            return false;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::insertChild( unsigned int index, Dragger* child )
{
    return osg::Group::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
const bool ManipulatorManager::IsEnabled() const
{
    return m_enabled;
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
void ManipulatorManager::UpdateAssociatedTransforms()
{
    /*
    //Set all associated node's transforms
    std::vector< osg::Transform* >::const_iterator itr =
        m_associatedTransforms.begin();
    for( itr; itr != m_associatedTransforms.end(); ++itr )
    {
        osg::Transform* transform = *itr;
        std::map< osg::Transform*, std::pair< osg::Matrixd, osg::Matrixd > >::const_iterator transformMatrices =
            m_associatedMatrices.find( transform );
        if( transformMatrices == m_associatedMatrices.end() )
        {
            //Error output
            break;
        }

        const osg::Matrixd& localToWorld = transformMatrices->second.first;
        const osg::Matrixd& worldToLocal = transformMatrices->second.second;

        osg::MatrixTransform* mt( NULL );
        osg::PositionAttitudeTransform* pat( NULL );
        osgBullet::AbsoluteModelTransform* amt( NULL );
        if( mt = transform->asMatrixTransform() )
        {
            const osg::Matrix& currentMatrix = mt->getMatrix();
            mt->setMatrix(
                localToWorld *
                osg::Matrix::translate( deltaTranslation ) * currentMatrix *
                worldToLocal );
        }
        else if( pat = transform->asPositionAttitudeTransform() )
        {
            const osg::Vec3d& currentPosition = pat->getPosition();
            osg::Vec3d newTranslation = currentPosition;
            newTranslation = newTranslation * localToWorld; 
            newTranslation += deltaTranslation;
            newTranslation = newTranslation * worldToLocal;
            pat->setPosition( newTranslation );
        }
        else if( amt = dynamic_cast< osgBullet::AbsoluteModelTransform* >( transform ) )
        {
            ;
        }
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
