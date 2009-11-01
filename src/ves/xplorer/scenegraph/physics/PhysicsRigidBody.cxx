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
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
//#include <ves/xplorer/scenegraph/physics/vesMotionState.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h>

#include <BulletCollision/CollisionShapes/btCompoundShape.h>
#include <BulletCollision/CollisionShapes/btCollisionShape.h>
#include <BulletCollision/CollisionShapes/btBoxShape.h>
#include <BulletCollision/CollisionShapes/btSphereShape.h>
#include <BulletCollision/CollisionShapes/btTriangleMesh.h>
#include <BulletCollision/CollisionShapes/btBvhTriangleMeshShape.h>
#include <BulletCollision/CollisionShapes/btConvexTriangleMeshShape.h>

#include <osgbBullet/CollisionShapes.h>
#include <osgbBullet/MotionState.h>
#include <osgwTools/AbsoluteModelTransform.h>
#include <osgbBulletPlus/OSGToCollada.h>
#include <osgbBullet/ColladaUtils.h>
#include <osgbBullet/Utils.h>
#include <osgbBullet/RefRigidBody.h>

#include <osg/io_utils>
#include <osg/ComputeBoundsVisitor>
#include <osg/BoundingBox>
#include <osg/PositionAttitudeTransform>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::PhysicsRigidBody( osg::Node* node,
                                    PhysicsSimulator* physicsSimulator )
    :
    mStoreCollisions( false ),
    mMass( 1.0 ),
    mFriction( 0.5 ),
    mRestitution( 0.5 ),
    mPhysicsSimulator( physicsSimulator ),
    mOSGToBullet( node ),
    mRB( 0 ),
    mDebugBoundaries( false )
{
    std::cout << "|\tPhysicsRigidBody: Just initializing physics variables." 
        << std::endl;
    std::cout << "|\t\tNode name: " << mOSGToBullet->getName() << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::~PhysicsRigidBody()
{
    ClearCollisions();
    //delete m_collisionShape;
    CleanRigidBody();
}
////////////////////////////////////////////////////////////////////////////////
const std::multimap< PhysicsRigidBody*, btVector3 >& PhysicsRigidBody::
    GetCollisions()
{
    return mCollisions;
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* PhysicsRigidBody::GetbtRigidBody()
{
    return mRB;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetMass( float mass )
{
    mMass = mass;
    SetMassProps();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetFriction( float friction )
{
    mFriction = friction;
    SetMassProps();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetRestitution( float restitution )
{
    mRestitution = restitution;
    SetMassProps();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetStoreCollisions( bool storeCollisions )
{
    if( storeCollisions == false )
    {
        ClearCollisions();
    }

    mStoreCollisions = storeCollisions;
}
////////////////////////////////////////////////////////////////////////////////
bool PhysicsRigidBody::HasCollisions()
{
    return !mCollisions.empty();
}
////////////////////////////////////////////////////////////////////////////////
bool PhysicsRigidBody::IsStoringCollisions()
{
    return mStoreCollisions;
}
////////////////////////////////////////////////////////////////////////////////
bool PhysicsRigidBody::CollisionInquiry( PhysicsRigidBody* physicsRigidBody )
{
    int count = mCollisions.count( physicsRigidBody );
    if( count > 0 )
    {
        return true;
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SetMassProps( bool dynamic )
{
    if( !mRB )
    {
        return;
    }

    mRB->setRestitution( mRestitution );
    mRB->setFriction( mFriction );

    //btRigidBody* is dynamic if and only if mass is non zero, otherwise static
    //if( dynamic )
    {
        dynamic = ( mMass != 0.0f );
    }

    btVector3 localInertia( 0.0, 0.0, 0.0 );
    if( dynamic )
    {
        mRB->getCollisionShape()->calculateLocalInertia( mMass, localInertia );
        mRB->setMassProps( mMass, localInertia );
    }
    else
    {
        mRB->setMassProps( 0.0, localInertia );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::PushBackCollision(
    PhysicsRigidBody* physicsRigidBody, const btVector3& location )
{
    mCollisions.insert( std::make_pair( physicsRigidBody, location ) );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::ClearCollisions()
{
    mCollisions.clear();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::CleanRigidBody()
{
    if( mRB )
    {
        //cleanup in the reverse order of creation/initialization
        btDynamicsWorld* dw = mPhysicsSimulator->GetDynamicsWorld();
        //remove the rigidbodies from the dynamics world and delete them
        dw->removeRigidBody( mRB );
        
        {
            btCollisionShape* tempShape = mRB->getCollisionShape();
            delete tempShape;
            std::cout << "|\tDeleting collision shape. " << std::endl;
        }
        
        {
            btMotionState* tempMS = mRB->getMotionState();
            delete tempMS;
            std::cout << "|\tDeleting motion state. " << std::endl;
        }
        
        delete mRB;
        mRB = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::BoundingBoxShape()
{
    CustomShape( BOX_SHAPE_PROXYTYPE, true );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SphereShape( double radius )
{
    if( radius == 0 )
    {
        CustomShape( SPHERE_SHAPE_PROXYTYPE, true );
    }
    else
    {
        btCollisionShape* collisionShape = new btSphereShape( radius );
        UserDefinedShape( collisionShape );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::UserDefinedShape( btCollisionShape* collisionShape )
{
    CleanRigidBody();

    bool dynamic = ( mMass != 0.0f );
    
    btVector3 localInertia( 0.0, 0.0, 0.0 );
    if( dynamic )
    {
        collisionShape->calculateLocalInertia( mMass, localInertia );
    }

    //Create a rigid body by default like we used to
    mRB = new btRigidBody( 
            btRigidBody::btRigidBodyConstructionInfo(
            btScalar( mMass ),                              //mass
            new osgbBullet::MotionState(),                   //motionState
            collisionShape,                                 //collisionShape
            localInertia ) );                               //localInertia
                                                          
    mRB->setMassProps( mMass, localInertia );
    
    RegisterRigidBody( mRB );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::RegisterRigidBody( btRigidBody* rigidBody )
{
    rigidBody->setUserPointer( this );
    ///Look at CCD demo in Demos/CcdPhysicsDemo/CcdPhysicsDemo.cpp
    ///http://www.bulletphysics.com/mediawiki-1.5.8/index.php?title=Anti_tunneling_by_Motion_Clamping
    // Only do CCD if  motion in one timestep (1.f/60.f) exceeds CUBE_HALF_EXTENTS
    //osg::BoundingSphere bs = mOSGToBullet->getBound();
    //rigidBody->setCcdMotionThreshold( bs.radius()*0.5 );

    //Experimental: better estimation of CCD Time of Impact:
    //rigidBody->setCcdSweptSphereRadius( 0.2*bs.radius() );
    
    //rigidBody->setActivationState( DISABLE_DEACTIVATION );

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( rigidBody );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::StaticConcaveShape()
{
    mMass = 0.0f;
    CustomShape( TRIANGLE_MESH_SHAPE_PROXYTYPE, false );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::ConvexShape()
{
    CustomShape( CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE, false );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::CustomShape( const BroadphaseNativeTypes shapeType, const bool overall, const std::string& decimation )
{
    CleanRigidBody();

    LocalToWorldNodePath ltw( mOSGToBullet.get(), 
        ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
    LocalToWorldNodePath::NodeAndPathList npl = ltw.GetLocalToWorldNodePath();

    if( npl.size() == 0 )
    {
        std::cerr << "|\tPhysicsRigidBody : File " << mOSGToBullet->getName() 
            << " not on the graph yet." << std::endl
            << "|\tTo enable physics the osg::Node must be on the graph." 
            << std::endl;
        return;
    }
    osg::Group* stopNode;
    osg::NodePath np;
    stopNode = static_cast< osg::Group* >( npl[ 0 ].first );
    np = npl[ 0 ].second;
    //We set mass props here so you MUST set mass before creating the shape
    {
        //std::cout << typeid( *mOSGToBullet.get() ).name() << std::endl;
        std::cout << "|\tMake a new btRigidBody for " << mOSGToBullet->getName() << std::endl;
        osg::ref_ptr< osg::PositionAttitudeTransform > tempSubgraph = 
            new osg::PositionAttitudeTransform( *static_cast< osg::PositionAttitudeTransform* >( mOSGToBullet.get() ), 
            osg::CopyOp::DEEP_COPY_ALL );            
        osgbBulletPlus::OSGToCollada converter;
        converter.setSceneGraph( tempSubgraph.get() );
        converter.setShapeType( shapeType );
        converter.setMass( mMass );
        converter.setOverall( overall );
        if( (shapeType == CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE) || 
            (shapeType == TRIANGLE_MESH_SHAPE_PROXYTYPE) )
        {
            //If decimation is exact we do nothing
            if( decimation == "High" )
            {
                converter.setDecimateParamaters( 0.10 );
            }
            else if( decimation == "Medium" )
            {
                converter.setDecimateParamaters( 0.45 );
            }
            else if( decimation == "Low" )
            {
                converter.setDecimateParamaters( 0.80 );
            }
            //converter.setSimplifyPercent( simplifyPercent );
        }
        //converter.setAxis( axis );
        converter.convert("");
       
        mRB = converter.getRigidBody();
        std::cout << "|\tJust finished creating a new btRigidBody." << std::endl;
    }
    
    osg::Group* parent = stopNode->getParent( 0 );
    osg::ref_ptr< osgwTools::AbsoluteModelTransform > amt = 
        dynamic_cast< osgwTools::AbsoluteModelTransform* >( parent );
    if( !amt.valid() )
    {
        amt = new osgwTools::AbsoluteModelTransform();
        amt->setName( "Physics AMT" );
        amt->setDataVariance( osg::Object::DYNAMIC );
        amt->addChild( mOSGToBullet.get() );
        
        parent->addChild( amt.get() );
        parent->removeChild( mOSGToBullet.get() );
    }
    const std::string dcsName = mOSGToBullet->getName();
    if( dcsName.empty() )
    {
        amt->setName( "AMT_" + dcsName );
    }
    osg::ref_ptr< osgbBullet::RefRigidBody > tempRB = new osgbBullet::RefRigidBody( mRB );
    amt->setUserData( tempRB.get() );

    mRB->setRestitution( mRestitution );
    mRB->setFriction( mFriction );
    
    osgbBullet::MotionState* motion = new osgbBullet::MotionState();
    //osgbBullet::MotionState* motion = dynamic_cast< osgbBullet::MotionState* >( mRB->getMotionState() );
    motion->setTransform( amt.get() );

    osg::BoundingSphere bs = mOSGToBullet->getBound();
    /*osg::ComputeBoundsVisitor cbbv( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN );
    mOSGToBullet->accept(cbbv);
    osg::BoundingBox bb = cbbv.getBoundingBox();
    std::cout << bb.center() << std::endl;
    std::cout << bb.radius() << std::endl;
    std::cout << bb._min << std::endl;
    std::cout << bb._max << std::endl;
    std::cout << bs.center() << std::endl;
    std::cout << bs.radius() << std::endl;*/
    // Add visual rep of Bullet Collision shape.
    /*osg::Node* visNode = osgbBullet::osgNodeFromBtCollisionShape( rb->getCollisionShape() );
    if( visNode != NULL )
    {
    osgbBullet::AbsoluteModelTransform* dmt = new osgbBullet::AbsoluteModelTransform;
    dmt->addChild( visNode );
    motion->setDebugTransform( dmt );
    _debugBullet.addDynamic( dmt );
    }*/
    
    osg::Matrix m;
    if( np.size() > 0 )
    { 
        m = osg::computeLocalToWorld( np );
    }
    //std::cout << "|\tParent Transform " << m << std::endl;
    //osg::Vec3d tempScale = m.getScale();
    motion->setParentTransform( m );
    motion->setCenterOfMass( bs.center() );
    osgbBullet::MotionState* tempMS = dynamic_cast< osgbBullet::MotionState* >( mRB->getMotionState() );
    if( tempMS )
    {
        std::cout << "|\tDeleting old motion state. " << std::endl;
        delete tempMS;
    }

    mRB->setMotionState( motion );
    //mRB->getCollisionShape()->setLocalScaling( 
    //    btVector3( tempScale.x(), tempScale.y(), tempScale.z() ) );
    //std::cout << tempScale << std::endl;
    /*std::cout << " center " << bs.center() << std::endl;
    std::cout << " center " << bs.radius() << std::endl;
    std::cout << " matrix " << m << std::endl;*/
    
    /*if( mMass != 0 )
    {
        setActivationState( DISABLE_DEACTIVATION );
    }*/
    
    RegisterRigidBody( mRB );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::CreateRigidBody( const std::string& lod, const std::string& motion, const std::string& mesh, const std::string& decimation )
{
    std::cout << "|\tPhysics parameters : " 
        << lod << " " << motion << " " << mesh << " " << decimation << std::endl;

    bool overall = false;
    if( lod == "Overall" )
    {
        overall = true;
    }
    
    if( motion == "Static" )
    {
        mMass = 0.0f;
    }
    std::cout << "|\t\tMesh " << mesh << " Overall " << overall << std::endl;
    
    if( mesh == "Box" )
    {
        CustomShape( BOX_SHAPE_PROXYTYPE, overall, decimation );
    }
    else if( mesh == "Sphere" )
    {
        CustomShape( SPHERE_SHAPE_PROXYTYPE, overall, decimation );
    }
    else if( mesh == "Cylinder" )
    {
        CustomShape( CYLINDER_SHAPE_PROXYTYPE, overall, decimation );
    }
    else
    {
        if( motion == "Static" )
        {
            CustomShape( TRIANGLE_MESH_SHAPE_PROXYTYPE, overall, decimation );
        }
        else
        {
            CustomShape( CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE, overall, decimation );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
