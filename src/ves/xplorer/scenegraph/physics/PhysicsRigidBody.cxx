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

#include <osgBullet/CollisionShape.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/OSGToCollada.h>
#include <osgBullet/DebugBullet.h>
#include <osgBullet/ColladaUtils.h>
#include <osgBullet/Utils.h>
#include <osgBullet/RigidBody.h>

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
        /*,
#if ( BULLET_MAJOR_VERSION >= 2 ) && ( BULLET_MINOR_VERSION > 65 )
        btRigidBody( btRigidBody::btRigidBodyConstructionInfo(
                     btScalar( mMass ),                         //mass
                     mVESMotionState = new osgBullet::MotionState(),    //motionState
                     0,                                         //collisionShape
                     btVector3( 0.0f, 0.0f, 0.0f ) ) )          //localInertia
#else
        btRigidBody( btScalar( mMass ),                         //mass
                     mVESMotionState = new osgBullet::MotionState(),    //motionState
                     0,                                         //collisionShape
                     btVector3( 0.0f, 0.0f, 0.0f ),             //localInertia
                     btScalar( 0.0f ),                          //linearDamping
                     btScalar( 0.0f ),                          //angularDamping
                     btScalar( 0.5f ),                          //friction
                     btScalar( 0.0f ) )                         //restitution
#endif*/
{
    //BoundingBoxShape();
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

        /*if( mDebugBoundaries )
        {
            osgBullet::MotionState* motion = 
                static_cast< osgBullet::MotionState* >( mRB->getMotionState() );
            osg::ref_ptr< osg::Transform > dbgNode = 
                motion->getDebugTransform();
            if( dbgNode.valid() )
            {
                mPhysicsSimulator->GetDebugBullet()->
                        remove( dbgNode.get() );
            }
        }*/
        
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
    /*if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    osg::BoundingBox bb = mOSGToBullet->GetBoundingBox();
    m_collisionShape = new btBoxShape(
                           btVector3( ( bb.xMax() - bb.xMin() ) * 0.5f,
                                      ( bb.yMax() - bb.yMin() ) * 0.5f,
                                      ( bb.zMax() - bb.zMin() ) * 0.5f ) );

    SetMassProps();

    if( mMass != 0 )
    {
        setActivationState( DISABLE_DEACTIVATION );
    }

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );*/
    CustomShape( BOX_SHAPE_PROXYTYPE, true );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::SphereShape( double radius )
{
    /*if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    if( radius == 0 )
    {
        m_collisionShape = new btSphereShape( mOSGToBullet->GetBoundingSphere().radius() );
    }
    else
    {
        m_collisionShape = new btSphereShape( radius );
    }

    SetMassProps();

    if( mMass != 0 )
    {
        setActivationState( DISABLE_DEACTIVATION );
    }

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );*/
    
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
            new osgBullet::MotionState(),                   //motionState
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
    
    /*if( mDebugBoundaries )
    {
        //Setup debug display
        // Add visual rep of Bullet Collision shape.
        osg::Node* visNode = 
        osgBullet::osgNodeFromBtCollisionShape( rigidBody->getCollisionShape() );
        if( visNode != NULL )
        {
            osgBullet::MotionState* motion = 
            static_cast< osgBullet::MotionState* >( rigidBody->getMotionState() );
            osgBullet::AbsoluteModelTransform* dmt = 
            new osgBullet::AbsoluteModelTransform();
            dmt->addChild( visNode );
            motion->setDebugTransform( dmt );
            mPhysicsSimulator->GetDebugBullet()->addDynamic( dmt );
            motion->resetTransform();
        }
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::StaticConcaveShape()
{
    /*if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_collisionShape = new btBvhTriangleMeshShape( mOSGToBullet->GetTriangleMesh(), false );

    SetMassProps( false );

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );*/
    mMass = 0.0f;
    CustomShape( TRIANGLE_MESH_SHAPE_PROXYTYPE, false );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::ConvexShape()
{
    /*if( this )
    {
        mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( this );
    }

    if( m_collisionShape )
    {
        delete m_collisionShape;
        m_collisionShape = 0;
    }

    m_collisionShape = new btConvexTriangleMeshShape( mOSGToBullet->GetTriangleMesh() );

    SetMassProps();

    if( mMass != 0 )
    {
        setActivationState( DISABLE_DEACTIVATION );
    }

    mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( this );*/
    CustomShape( CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE, false );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::CustomShape( const BroadphaseNativeTypes shapeType, const bool overall )
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
        osgBullet::OSGToCollada converter;
        converter.setSceneGraph( tempSubgraph.get() );
        converter.setShapeType( shapeType );
        converter.setMass( mMass );
        converter.setOverall( overall );
        //converter.setSimplifyPercent( simplifyPercent );
        //converter.setAxis( axis );
        converter.convert("");
       
        mRB = converter.getRigidBody();
        std::cout << "|\tJust finished creating a new btRigidBody." << std::endl;
    }
    
    osg::Group* parent = stopNode->getParent( 0 );
    osg::ref_ptr< osgBullet::AbsoluteModelTransform > amt = 
        dynamic_cast< osgBullet::AbsoluteModelTransform* >( parent );
    if( !amt.valid() )
    {
        amt = new osgBullet::AbsoluteModelTransform();
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
    osg::ref_ptr< osgBullet::RigidBody > tempRB = new osgBullet::RigidBody( mRB );
    amt->setUserData( tempRB.get() );

    mRB->setRestitution( mRestitution );
    mRB->setFriction( mFriction );
    
    osgBullet::MotionState* motion = new osgBullet::MotionState();
    //osgBullet::MotionState* motion = dynamic_cast< osgBullet::MotionState* >( mRB->getMotionState() );
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
    /*osg::Node* visNode = osgBullet::osgNodeFromBtCollisionShape( rb->getCollisionShape() );
    if( visNode != NULL )
    {
    osgBullet::AbsoluteModelTransform* dmt = new osgBullet::AbsoluteModelTransform;
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
    osgBullet::MotionState* tempMS = dynamic_cast< osgBullet::MotionState* >( mRB->getMotionState() );
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
void PhysicsRigidBody::CreateRigidBody( const std::string& lod, const std::string& motion, const std::string& mesh )
{
    std::cout << "|\tPhysics parameters : " 
        << lod << " " << motion << " " << mesh << std::endl;

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
        CustomShape( BOX_SHAPE_PROXYTYPE, overall );
    }
    else if( mesh == "Sphere" )
    {
        CustomShape( SPHERE_SHAPE_PROXYTYPE, overall );
    }
    else if( mesh == "Cylinder" )
    {
        CustomShape( CYLINDER_SHAPE_PROXYTYPE, overall );
    }
    else
    {
        if( motion == "Static" )
        {
            CustomShape( TRIANGLE_MESH_SHAPE_PROXYTYPE, overall );
        }
        else
        {
            CustomShape( CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE, overall );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
