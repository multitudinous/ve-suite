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

// --- VE-Suite Includes --- //
#include <ves/xplorer/device/Device.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

#ifdef MINERVA_GIS_SUPPORT
//These includes must be below the gmlt matrix convert header
#include <ves/xplorer/minerva/MinervaManager.h>
#include <Minerva/Core/TileEngine/Body.h>
#include <Minerva/Core/TileEngine/LandModel.h>
#endif

// --- OSG Includes --- //
#include <osg/Polytope>
#include <osg/LineSegment>
#include <osg/Material>

#include <osgUtil/IntersectVisitor>
#include <osgUtil/IntersectionVisitor>
#include <osgUtil/PolytopeIntersector>

#include <boost/concept_check.hpp>

using namespace ves::xplorer;
using namespace ves::xplorer::device;

////////////////////////////////////////////////////////////////////////////////
Device::Device( const Device::Type& type )
    :
    GlobalBase(),
    m_enabled( false ),
    m_type( type ),
    mCenterPointThreshold( NULL ),
    mCenterPointJump( NULL ),
    mResetPosition( NULL ),
    mCenterPoint( NULL ),
    mResetAxis( NULL ),
    m_physicsSimulator( *scenegraph::PhysicsSimulator::instance() ),
    m_sceneManager( *scenegraph::SceneManager::instance() ),
    m_characterController( m_sceneManager.GetCharacterController() ),
    m_manipulatorManager( m_sceneManager.GetManipulatorManager() ),
    m_cameraManager( m_sceneManager.GetCameraManager() )
{
    CONNECTSIGNALS_1( "%NavigationZEqual0Lock",
                      void( bool const & enable ),
                      &Device::UpdateZEqualZero,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_1( "%NavigationZGreater0Lock",
                      void( bool const & enable ),
                      &Device::UpdateZGreaterZero,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
Device::Device( const Device& device )
    :
    GlobalBase( device ),
    m_type( device.m_type ),
    m_physicsSimulator( device.m_physicsSimulator ),
    m_sceneManager( device.m_sceneManager ),
    m_characterController( device.m_characterController ),
    m_manipulatorManager( device.m_manipulatorManager ),
    m_cameraManager( device.m_cameraManager )
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
Pointer* Device::AsPointer()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
GameControllerCallbacks* Device::AsGameController()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
const Device::Type& Device::GetType() const
{
    return m_type;
}
////////////////////////////////////////////////////////////////////////////////
void Device::ProcessEvents( ves::open::xml::CommandPtr command )
{
    boost::ignore_unused_variable_warning( command );
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
    m_sceneManager.GetRootNode()->accept( intersect_visitor );

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

    m_sceneManager.GetActiveSwitchNode()->accept( intersectionVisitor );
    if( headCollider->containsIntersections() )
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
    boost::ignore_unused_variable_warning( startPoint );
    boost::ignore_unused_variable_warning( endPoint );
}
////////////////////////////////////////////////////////////////////////////////
void Device::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    boost::ignore_unused_variable_warning( startPoint );
    boost::ignore_unused_variable_warning( endPoint );
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetResetWorldPosition(
    osg::Quat* quat, std::vector< double >* pos )
{
    mResetAxis = quat;
    mResetPosition = pos;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetSubZeroFlag( int input )
{
    m_subzeroFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Device::SetZEqualsZeroFlag( int input )
{
    m_zEqualsZeroFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
void Device::EnsureCameraStaysAboveGround( const gmtl::Matrix44d& headMatrix, double* worldTranslation, const osg::Quat& world_quat, int m_subzeroFlag, int m_zEqualsZeroFlag )
{
#ifdef MINERVA_GIS_SUPPORT
    Minerva::Core::TileEngine::Body* tileEngineBody =
        ves::xplorer::minerva::MinervaManager::instance()->GetTileEngineBody();
    if( tileEngineBody )
    {
        Minerva::Core::TileEngine::LandModel* landModel =
            tileEngineBody->landModel();

        osg::Vec3d t( -worldTranslation[0], -worldTranslation[1], -worldTranslation[2] );
        osg::Vec3d position( world_quat.inverse() * t );

        double lat, lon, elevation;
        landModel->xyzToLatLonHeight( position[0], position[1],
                                      position[2], lat, lon, elevation );
        double earthElevation = tileEngineBody->elevationAtLatLong( lat, lon );
        gmtl::Point3d jugglerHeadPoint;
        jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( headMatrix );
        earthElevation += jugglerHeadPoint[ 1 ];

        const double minimunDistanceAboveGround( 2.0 );
        earthElevation += minimunDistanceAboveGround;

        if( earthElevation > elevation )
        {
            elevation = earthElevation;
            landModel->latLonHeightToXYZ( lat, lon, elevation, position[0],
                                          position[1], position[2] );
            position = world_quat * position;
            worldTranslation[0] = -position[0];
            worldTranslation[1] = -position[1];
            worldTranslation[2] = -position[2];
        }
    }
    else
#else
    boost::ignore_unused_variable_warning( headMatrix );
    boost::ignore_unused_variable_warning( world_quat );
#endif
        //If the GIS rendering engine is on then we do not want to lock to z > 0

        if( m_subzeroFlag )
        {
            if( worldTranslation[ 1 ] > 0 )
            {
                worldTranslation[ 1 ] = 0;
            }
        }
    if( m_zEqualsZeroFlag )
    {
        worldTranslation[ 1 ] = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Device::UpdateZEqualZero( bool const& enable )
{
    SetZEqualsZeroFlag( enable );
}
////////////////////////////////////////////////////////////////////////////////
void Device::UpdateZGreaterZero( bool const& enable )
{
    SetSubZeroFlag( enable );
}
////////////////////////////////////////////////////////////////////////////////
