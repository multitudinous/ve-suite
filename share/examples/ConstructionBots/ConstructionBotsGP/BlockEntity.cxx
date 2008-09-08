/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

// --- My Includes --- //
#include "BlockEntity.h"
#include "Block.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/Sound.h>

#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- OSG Includes --- //
#include <osgUtil/IntersectionVisitor>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>
#include <BulletDynamics/ConstraintSolver/btGeneric6DofConstraint.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace bots;

const double piDivOneEighty = 0.0174532925;

////////////////////////////////////////////////////////////////////////////////
BlockEntity::BlockEntity(
    bots::Block* block,
    ves::xplorer::scenegraph::DCS* pluginDCS,
#ifdef VE_SOUND
    osgAL::SoundManager* soundManager,
#endif
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
    :
    CADEntity( block, pluginDCS, physicsSimulator ),
    mAttached( false ),
    mSiteColor( 0.2, 0.2, 0.2, 1.0 ),
    mAttachColor( 0.0, 1.0, 0.0, 1.0 ),
    mNoAttachColor( 1.0, 0.0, 0.0, 1.0 ),
    mPluginDCS( pluginDCS ),
#ifdef VE_SOUND
    mPickUpBlockSound( new ves::xplorer::scenegraph::Sound( 
                           "PickUpBlockSound", GetDCS(), soundManager ) ),
#endif
    mBlockGeometry( block ),
    mConstraint( 0 ),
    mLocation( 0, 0 ),
    mLocalPositions( new osg::Vec3Array() ),
    mLineSegmentIntersector( new osgUtil::LineSegmentIntersector(
                                 osg::Vec3d( 0, 0, 0 ), osg::Vec3d( 0, 0, 0 ) ) )
{
    //Initialize side attachments
    mConnectedBlocks[ 0 ] = NULL;
    mConnectedBlocks[ 1 ] = NULL;
    mConnectedBlocks[ 2 ] = NULL;
    mConnectedBlocks[ 3 ] = NULL;

    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
BlockEntity::~BlockEntity()
{
    if( mConstraint )
    {
        if( mPhysicsSimulator )
        {
            mPhysicsSimulator->GetDynamicsWorld()->removeConstraint(
                mConstraint );
        }

        delete mConstraint;
    }
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::Initialize()
{
#ifdef VE_SOUND
    try
    {
        mPickUpBlockSound->LoadFile( "Sounds/PickUpBlock.wav" );
    }
    catch( ... )
    {
        std::cerr << "Could not load sounds" << std::endl;
    }
#endif

    CalculateLocalPositions();
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::CalculateLocalPositions()
{
    mLocalPositions->resize( 8 );

    double blockHalfWidth = 0.5;
    osg::Vec3d startPoint, endPoint;
    startPoint.set( blockHalfWidth - 0.1, 0.0, 0.0 );
    endPoint.set( blockHalfWidth + 0.1, 0.0, 0.0 );

    //Rotate vector about point( 0, 0, 0 ) by theta
    //x' = x * cos( theta ) - y * sin( theta );
    //y' = x * sin( theta ) + y * cos( theta );
    for( int i = 0; i < 4; ++i )
    {
        double x, y, xNew, yNew;
        double cosTheta = cos( i * 90 * piDivOneEighty );
        double sinTheta = sin( i * 90 * piDivOneEighty );

        x = startPoint.x();
        y = startPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mLocalPositions)[ i * 2 ] = osg::Vec3d( xNew, yNew, 0 );

        x = endPoint.x();
        y = endPoint.y();
        xNew = ( x * cosTheta ) - ( y * sinTheta );
        yNew = ( x * sinTheta ) + ( y * cosTheta );
        (*mLocalPositions)[ i * 2 + 1 ] = osg::Vec3d( xNew, yNew, 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::AttachUpdate()
{
    //Update the connected blocks for this and neighbors
    ConnectionDetection();

    //Get coordinates and occupancy matrix from neighbors
    std::map< unsigned int, bots::BlockEntity* >::const_iterator itr;
    itr = mConnectedBlocks.begin();
    for( itr = mConnectedBlocks.begin(); itr != mConnectedBlocks.end(); ++itr )
    {
        if( itr->second )
        {
            //Get this block's location from neighbor
            mLocation = itr->second->GetLocation();
            switch( itr->first )
            {
                case 0:
                {
                    mLocation.first -= 1;
                }
                break;

                case 1:
                {
                    mLocation.second -= 1;
                }
                break;

                case 2:
                {
                    mLocation.first += 1;
                }
                break;

                case 3:
                {
                    mLocation.second += 1;
                }
                break;
            }

            //Get a copy of the occupancy from neighbor
            SetOccupancyMatrix( itr->second->GetOccupancyMatrix() );

            break;
        }
    }

    //Self align w/ blocks
    GetPhysicsRigidBody()->StaticConcaveShape();
    double* alignedPosition = GetDCS()->GetVETranslationArray();
    alignedPosition[ 0 ] = mLocation.first;
    alignedPosition[ 1 ] = mLocation.second;
    GetDCS()->SetTranslationArray( alignedPosition );

    //Change the block color
    for( int i = 4; i < 10; ++i )
    {
        mBlockGeometry->SetColor( i, mSiteColor );
    }

    mAttached = true;
    std::map< std::pair< int, int >,
              std::pair< bool, bool > >::iterator oMatItr =
        mOccupancyMatrix->find( mLocation );
    if( oMatItr != mOccupancyMatrix->end() )
    {
        oMatItr->second.second = true;
    }
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::UpdateSideStates()
{
    std::map< osg::Drawable*, bool >::iterator itrSS;
    std::map< unsigned int, bots::BlockEntity* >::const_iterator itr;
    for( itrSS = mSideStates.begin(), itr = mConnectedBlocks.begin();
         itrSS != mSideStates.end(), itr != mConnectedBlocks.end();
         ++itrSS, ++itr )
    {
        if( !itr->second )
        {
            itrSS->second = mNeighborOccupancy[ itr->first ];
            if( itrSS->second == false )
            {
                mBlockGeometry->SetColor( itr->first, mSiteColor );
            }
            else
            {
                //Prove that you can't attach block
                //This is the "separation rule"
                bool canAttachBlock( true );

                osg::Vec3 tempVector =
                    mLocalPositions->at( itr->first * 2 + 1 );
                tempVector.normalize();
                std::pair< int, int > location;
                std::pair< int, int > posNinety( -tempVector.y(),
                                                  tempVector.x() );
                std::pair< int, int > negNinety(  tempVector.y(),
                                                 -tempVector.x() );

                //Go in positive direction first
                bool emptyOccupance( false );
                location = mLocation;
                //Static cast to remove small precision errors
                location.first += static_cast< int >( tempVector.x() );
                location.second += static_cast< int >( tempVector.y() );
                do
                {
                    location.first += posNinety.first;
                    location.second += posNinety.second;

                    if( (*mOccupancyMatrix)[ location ].first )
                    {
                        if( (*mOccupancyMatrix)[ location ].second )
                        {
                            if( emptyOccupance )
                            {
                                canAttachBlock = false;
                            }
                        }
                        else
                        {
                            emptyOccupance = true;
                        }
                    }
                    else
                    {
                        emptyOccupance = false;
                    }
                }
                while( emptyOccupance );

                //Go in negative direction if passed first test
                if( canAttachBlock )
                {
                    emptyOccupance = false;
                    location = mLocation;
                    //Static cast to remove small precision errors
                    location.first += static_cast< int >( tempVector.x() );
                    location.second += static_cast< int >( tempVector.y() );
                    do
                    {
                        location.first += negNinety.first;
                        location.second += negNinety.second;

                        if( (*mOccupancyMatrix)[ location ].first )
                        {
                            if( (*mOccupancyMatrix)[ location ].second )
                            {
                                if( emptyOccupance )
                                {
                                    canAttachBlock = false;
                                }
                            }
                            else
                            {
                                emptyOccupance = true;
                            }
                        }
                        else
                        {
                            emptyOccupance = false;
                        }
                    }
                    while( emptyOccupance );
                }

                if( canAttachBlock )
                {
                    itrSS->second = true;
                    mBlockGeometry->SetColor( itr->first, mAttachColor );
                    
                }
                else
                {
                    itrSS->second = false;
                    mBlockGeometry->SetColor( itr->first, mNoAttachColor );
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool BlockEntity::IsAttached()
{
    return mAttached;
}
////////////////////////////////////////////////////////////////////////////////
bool BlockEntity::PermissionToAttach( osg::Drawable* drawable )
{
    std::map< osg::Drawable*, bool >::const_iterator itr =
        mSideStates.find( drawable );
    if( !drawable || itr == mSideStates.end() )
    {
        return false;
    }
    else
    {
        return itr->second;
    }
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::ConnectionDetection()
{
    osg::Vec3 blockPosition = mDCS->getPosition();
    for( unsigned int i = 0; i < 4; ++i )
    {
        osg::Vec3d startPoint = (*mLocalPositions)[ i * 2 ];
        osg::Vec3d endPoint = (*mLocalPositions)[ i * 2 + 1 ];

        startPoint += blockPosition;
        endPoint += blockPosition;

        mLineSegmentIntersector->reset();
        mLineSegmentIntersector->setStart( startPoint );
        mLineSegmentIntersector->setEnd( endPoint );

        osgUtil::IntersectionVisitor intersectionVisitor(
            mLineSegmentIntersector.get() );
        mPluginDCS->accept( intersectionVisitor );

        bool sideState( true );
        osg::ref_ptr< osg::Drawable > thisDrawable( NULL );
        if( mLineSegmentIntersector->containsIntersections() )
        {
            const osgUtil::LineSegmentIntersector::Intersections& intersections =
                mLineSegmentIntersector->getIntersections();

            std::multiset< osgUtil::LineSegmentIntersector::Intersection >::const_iterator itr;
            for( itr = intersections.begin(); itr != intersections.end(); ++itr )
            {
                osg::ref_ptr< osg::Drawable > drawable = itr->drawable;
                ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                    drawable->getParent( 0 ) );
                osg::ref_ptr< ves::xplorer::scenegraph::DCS > dcs =
                    static_cast< ves::xplorer::scenegraph::DCS* >(
                        parentVisitor.GetParentNode() );

                std::map< std::string, bots::BlockEntity* >::const_iterator bemItr =
                    mBlockEntityMap->find( dcs->GetName() );
                bots::BlockEntity* blockEntity( NULL );
                if( bemItr != mBlockEntityMap->end() )
                {
                    blockEntity = bemItr->second;
                }

                if( blockEntity )
                {
                    if( blockEntity != this )
                    {
                        unsigned int oppositeSide = i;
                        if( i > 1 )
                        {
                            oppositeSide -= 2;
                        }
                        else
                        {
                            oppositeSide += 2;
                        }

                        mBlockGeometry->SetColor( i, mSiteColor );
                        mConnectedBlocks[ i ] = blockEntity;
                        mConnectedBlocks[ i ]->SetBlockConnection(
                            oppositeSide, this );
                        mConnectedBlocks[ i ]->GetBlockGeometry()->SetColor(
                            oppositeSide, mSiteColor );
                        sideState = false;
                    }
                    else
                    {
                        thisDrawable = drawable;
                    }
                }
            }
        }

        if( thisDrawable.valid() )
        {
            mSideStates[ thisDrawable.get() ] = sideState;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bots::Block* BlockEntity::GetBlockGeometry()
{
    return mBlockGeometry.get();
}
////////////////////////////////////////////////////////////////////////////////
const std::pair< int, int >& BlockEntity::GetLocation()
{
    return mLocation;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::pair< int, int >,
          std::pair< bool, bool > >* BlockEntity::GetOccupancyMatrix()
{
    return mOccupancyMatrix;
}
////////////////////////////////////////////////////////////////////////////////
#ifdef VE_SOUND
ves::xplorer::scenegraph::Sound* const BlockEntity::GetPickUpBlockSound() const
{
    return mPickUpBlockSound;
}
#endif
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetBlockConnection(
    unsigned int side, bots::BlockEntity* blockEntity )
{
    mConnectedBlocks[ side ] = blockEntity;
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetBlockEntityMap(
    std::map< std::string, bots::BlockEntity* >* blockEntityMap )
{
    mBlockEntityMap = blockEntityMap;
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetConstraints( int gridSize )
{
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin( btVector3( 0.0, 0.0, 0.5 ) );

    //Must disable deactivation so constraint is always applied
    mPhysicsRigidBody->setActivationState( DISABLE_DEACTIVATION );
    btRigidBody* fixedBody = mPhysicsSimulator->CreateRigidBody( 0, trans, 0 );

    btTransform frameInA, frameInB;
    frameInA = btTransform::getIdentity();
    frameInB = btTransform::getIdentity();

#if( BULLET_MAJOR_VERSION >= 2 ) && ( BULLET_MINOR_VERSION > 61 )
    mConstraint = new btGeneric6DofConstraint(
        *mPhysicsRigidBody, *fixedBody, frameInA, frameInB, false );
#else
    mConstraint = new btGeneric6DofConstraint(
        *mPhysicsRigidBody, *fixedBody, frameInA, frameInB );
#endif

    //Fix the translation range for the agents
    //Give 0.5 units extra in xy plane for agent/wall collision
    //Give small z-range for agent/grid collision
    mConstraint->setLinearLowerLimit( btVector3( -100.0, -100.0, -100.0 ) );
    mConstraint->setLinearUpperLimit( btVector3(  100.0,  100.0,  100.0 ) );

    //Remove rotation from agents
    //Range should be small or singularities will 'explode' the constraint
    mConstraint->setAngularLowerLimit( btVector3( 0, 0, 0 ) );
    mConstraint->setAngularUpperLimit( btVector3( 0, 0, 0 ) );

    mPhysicsSimulator->GetDynamicsWorld()->addConstraint( mConstraint );
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetNameAndDescriptions( int number )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    mDCS->setDescriptions( descriptorsList );

    std::stringstream ss;
    ss << "Block" << number;
    std::cout << ss.str() << std::endl;
    mDCS->setName( ss.str() );
}
////////////////////////////////////////////////////////////////////////////////
void BlockEntity::SetOccupancyMatrix(
    std::map< std::pair< int, int >,
              std::pair< bool, bool > >* occupancyMatrix )
{
    mOccupancyMatrix = occupancyMatrix;

    //Store the neighbors occupation
    mNeighborOccupancy[ 0 ] =
        ( *mOccupancyMatrix )[ std::make_pair( mLocation.first + 1,
                                               mLocation.second ) ].first;
    mNeighborOccupancy[ 1 ] =
        ( *mOccupancyMatrix )[ std::make_pair( mLocation.first,
                                               mLocation.second + 1 ) ].first;
    mNeighborOccupancy[ 2 ] =
        ( *mOccupancyMatrix )[ std::make_pair( mLocation.first - 1,
                                               mLocation.second ) ].first;
    mNeighborOccupancy[ 3 ] =
        ( *mOccupancyMatrix )[ std::make_pair( mLocation.first,
                                               mLocation.second - 1 ) ].first;
}
////////////////////////////////////////////////////////////////////////////////
