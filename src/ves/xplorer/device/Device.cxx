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
#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

#ifdef QT_ON
#include <ves/conductor/qt/UIManager.h>
#endif
// --- OSG Includes --- //
#include <osg/Polytope>
#include <osg/LineSegment>
#include <osg/Material>

#include <osgUtil/IntersectVisitor>
#include <osgUtil/IntersectionVisitor>
#include <osgUtil/PolytopeIntersector>

using namespace ves::xplorer;
using namespace ves::xplorer::device;

////////////////////////////////////////////////////////////////////////////////
Device::Device( const Device::Type& type )
    :
    GlobalBase(),
    m_enabled( false ),
    m_type( type ),
    mCenterPoint( NULL ),
    mCenterPointThreshold( NULL ),
    mCenterPointJump( NULL ),
    mResetPosition( NULL ),
    mResetAxis( NULL ),
    m_physicsSimulator( *scenegraph::PhysicsSimulator::instance() ),
    m_sceneManager( *scenegraph::SceneManager::instance() ),
    m_characterController( *(m_sceneManager.GetCharacterController()) ),
    m_manipulatorManager( *(m_sceneManager.GetManipulatorManager()) )
#ifdef QT_ON
    ,
    m_uiManager( *ves::conductor::UIManager::instance() )
#endif
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Device::Device( const Device& device )
    :
    GlobalBase( device ),
    m_type( device.m_type ),
    m_physicsSimulator( device.m_physicsSimulator ),
    m_sceneManager( device.m_sceneManager ),
    m_characterController( device.m_characterController ),
    m_manipulatorManager( device.m_manipulatorManager )
#ifdef QT_ON
    ,
    m_uiManager( device.m_uiManager )
#endif
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Device::~Device()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Gloves* Device::AsGloves()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse* Device::AsKeyboardMouse()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Tablet* Device::AsTablet()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Wand* Device::AsWand()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
const Device::Type& Device::GetType() const
{
    return m_type;
}
////////////////////////////////////////////////////////////////////////////////
void Device::UpdateCommand()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetCenterPoint( gmtl::Point3d* centerPoint )
{
    mCenterPoint = centerPoint;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetCenterPointThreshold( double* threshold )
{
    mCenterPointThreshold = threshold;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetCenterPointJump( double* jump )
{
    mCenterPointJump = jump;
}
////////////////////////////////////////////////////////////////////////////////
void Device::ProcessSelection()
{
    osg::Vec3d start_point;
    osg::Vec3d end_point;
    SetStartEndPoint( &start_point, &end_point );

    osg::ref_ptr< osg::LineSegment > line_segment = new osg::LineSegment();
    line_segment->set( start_point, end_point );

    osgUtil::IntersectVisitor intersect_visitor;
    intersect_visitor.addLineSegment( line_segment.get() );

    //Add IntersectVisitor to RootNode so that all geometry is checked and no transforms are applied to LineSegment
    scenegraph::SceneManager::instance()->GetRootNode()->accept( intersect_visitor );

    osgUtil::IntersectVisitor::HitList hit_list;
    hit_list = intersect_visitor.getHitList( line_segment.get() );

    //Traversal part
    osgUtil::Hit objectHit;

    osg::ref_ptr< osg::Geode > selected_geometry;

    if( hit_list.empty() )
    {
        //return;
    }
    else
    {
        for( unsigned int i = 0; i < hit_list.size(); i++ )
        {
            objectHit = hit_list[i];
            /*
            if( objectHit._geode->getName() != laserName )
            {
               break;
            }
            */
        }

        if( objectHit._geode.valid() )
        {
            if( !objectHit._geode->getName().empty() )
            {
                if( /*objectHit._geode->getName() != laserName && */
                    objectHit._geode->getName() != "Root Node" )
                {
                    selected_geometry = objectHit._geode;
                    std::cout << objectHit._geode->getName() << std::endl;
                }
            }
            else
            {
                selected_geometry = objectHit._geode;
                std::cout << objectHit._geode->getParents().front()->getName() << std::endl;
            }
        }
    }

    DrawLine( start_point, end_point );
}
////////////////////////////////////////////////////////////////////////////////
bool Device::CheckCollisionsWithHead( osg::Vec3 headPositionInWorld )
{
    //Simple  box for the head/body 
    //Can make this a better represenation later
    //These objects can probably be moved to be members of this class
    osg::BoundingBox headBBox;
    headBBox.set( headPositionInWorld.x() - .75,
                  headPositionInWorld.y() - .75,
                  headPositionInWorld.z() - 5.0,
                  headPositionInWorld.x() + .75,
                  headPositionInWorld.y() + .75,
                  headPositionInWorld.z() + 5.0 );

    osg::Polytope polytope;
    polytope.setToBoundingBox( headBBox );
   
    osg::ref_ptr<osgUtil::PolytopeIntersector> headCollider =
                         new osgUtil::PolytopeIntersector( polytope );
    osgUtil::IntersectionVisitor intersectionVisitor( headCollider.get() );

    scenegraph::SceneManager::instance()->GetActiveSwitchNode()->accept( intersectionVisitor );
    if ( headCollider->containsIntersections() )
    {
        return true;
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
void Device::Enable( const bool& enable )
{
    m_enabled = enable;
}
////////////////////////////////////////////////////////////////////////////////
const bool& Device::IsEnabled()
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetStartEndPoint( osg::Vec3d* startPoint, osg::Vec3d* endPoint )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Device::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetResetWorldPosition(
    osg::Quat* quat, std::vector< double >* pos )
{
    mResetAxis = quat;
    mResetPosition = pos;
}
////////////////////////////////////////////////////////////////////////////////
