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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/GhostController.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

// --- OSG Includes --- //
#include <osg/io_utils>
#include <osg/ComputeBoundsVisitor>
#include <osg/BoundingBox>
#include <osg/PositionAttitudeTransform>

#include <osgDB/WriteFile>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h>

#include <BulletCollision/CollisionShapes/btCompoundShape.h>
#include <BulletCollision/CollisionShapes/btCollisionShape.h>
#include <BulletCollision/CollisionShapes/btBoxShape.h>
#include <BulletCollision/CollisionShapes/btSphereShape.h>
#include <BulletCollision/CollisionShapes/btTriangleMesh.h>
#include <BulletCollision/CollisionShapes/btBvhTriangleMeshShape.h>
#include <BulletCollision/CollisionShapes/btConvexTriangleMeshShape.h>

#include <BulletCollision/CollisionDispatch/btGhostObject.h>
#include <BulletCollision/CollisionDispatch/btInternalEdgeUtility.h>

// --- osgBullet Includes --- //
#include <osgbDynamics/MotionState.h>
#include <osgbCollision/CollisionShapes.h>
#include <osgbCollision/RefBulletObject.h>
#include <osgbDynamics/RigidBody.h>

#include <osgwTools/AbsoluteModelTransform.h>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::PhysicsRigidBody(
    osg::Node* node, PhysicsSimulator* physicsSimulator )
    :
    m_ghostControl( false ),
    mMass( 1.0 ),
    mFriction( 0.5 ),
    mRestitution( 0.5 ),
    mDebugBoundaries( false ),
    mRB( NULL ),
    m_physicsMaterial( NULL ),
    mOSGToBullet( node ),
    m_ghostController( new GhostController() ),
    m_physicsSimulator( *physicsSimulator ),
    m_dynamicsWorld( *(m_physicsSimulator.GetDynamicsWorld()) )
{
    std::cout << "|\tPhysicsRigidBody: Just initializing physics variables."
              << std::endl;
    std::cout << "|\t\tNode name: " << mOSGToBullet->getName() << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody::~PhysicsRigidBody()
{
    CleanRigidBody();
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
void PhysicsRigidBody::CleanRigidBody()
{
    if( mRB )
    {
        //cleanup in the reverse order of creation/initialization

        //remove the rigidbodies from the dynamics world and delete them
        m_dynamicsWorld.removeRigidBody( mRB );

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

        std::cout << "|\tNumber of constraints "
                  << mRB->getNumConstraintRefs() << std::endl;

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

    bool dynamic = ( mMass != 0.0 );

    btVector3 localInertia( 0.0, 0.0, 0.0 );
    if( dynamic )
    {
        collisionShape->calculateLocalInertia( mMass, localInertia );
    }

    //Create a rigid body by default like we used to
    mRB = new btRigidBody( 
            btRigidBody::btRigidBodyConstructionInfo(
            btScalar( mMass ),                              //mass
            new osgbDynamics::MotionState(),                   //motionState
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

    m_dynamicsWorld.addRigidBody( rigidBody );

    //In the future make static objects act like kinematic by interpolating the
    //transform
    //The drawback of this solution is we can't accurately detect static-static
    //collisions because static objects are not always convex
    //So we might have to live with static-static penetration
    m_ghostController->SetCollisionShape( rigidBody->getCollisionShape() );
    //Set the ms for the ghost object to update
    m_ghostController->SetMotionState( rigidBody->getMotionState() );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::StaticConcaveShape()
{
    mMass = 0.0;
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
    
    //Should we be ensuring that the top level node is toggled no before
    //we try to create physics with it???
    osg::Node::NodeMask tempMask = mOSGToBullet->getNodeMask();
    mOSGToBullet->setNodeMask( 1 );
    
    LocalToWorldNodePath ltw( mOSGToBullet.get(), 
        ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
    LocalToWorldNodePath::NodeAndPathList npl = ltw.GetLocalToWorldNodePath();
    
    //Now lets change the node back to how it was now that we are done
    //traversing it.
    mOSGToBullet->setNodeMask( tempMask );
    
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
    if( !dcsName.empty() )
    {
        amt->setName( "AMT_" + dcsName );
    }
    
    //osg::BoundingSphere bs = mOSGToBullet->getBound();
    osg::ComputeBoundsVisitor cbbv( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN );
    mOSGToBullet->accept(cbbv);
    osg::BoundingBox bb = cbbv.getBoundingBox();
    
    {        
        osg::ref_ptr< osgbDynamics::CreationRecord > cr = 
            new osgbDynamics::CreationRecord();
        cr->_sceneGraph = mOSGToBullet.get();
        cr->setCenterOfMass( bb.center() );
        cr->_shapeType = shapeType;
        cr->_restitution = mRestitution;
        cr->_friction = mFriction;
        cr->_mass = mMass;
        cr->_reductionLevel = osgbDynamics::CreationRecord::NONE;
        cr->_overall = overall;
        
        //If decimation is exact we do nothing
        if( decimation == "High" )
        {
            cr->_reductionLevel = osgbDynamics::CreationRecord::AGGRESSIVE;
        }
        else if( decimation == "Medium" )
        {
            cr->_reductionLevel = osgbDynamics::CreationRecord::INTERMEDIATE;
        }
        else if( decimation == "Low" )
        {
            cr->_reductionLevel = osgbDynamics::CreationRecord::MINIMAL;
        }
        
        mRB = osgbDynamics::createRigidBody( cr.get() );
        
        osg::ref_ptr< osgbCollision::RefRigidBody > tempRB =
            new osgbCollision::RefRigidBody( mRB );
        amt->setUserData( tempRB.get() );
    }
    
    //These are the default values for the sleeping parameters for island 
    //creation by the solver. By making these larger an object will go to 
    //sleep sooner. This can have a negative affect on the fidelity of the sim.
    //rbInfo.m_linearSleepingThreshold = btScalar(0.8);
    //rbInfo.m_angularSleepingThreshold = btScalar(1.f);
    mRB->setSleepingThresholds( 1.6, 2.0 );
    
    osgbDynamics::MotionState* motion = new osgbDynamics::MotionState();
    motion->setTransform( amt.get() );
    
    osg::Matrix m;
    if( np.size() > 0 )
    { 
        m = osg::computeLocalToWorld( np );
    }
    
    motion->setParentTransform( m );
    motion->setCenterOfMass( bb.center() );
    
    osgbDynamics::MotionState* tempMS = dynamic_cast< osgbDynamics::MotionState* >( mRB->getMotionState() );
    if( tempMS )
    {
        std::cout << "|\tDeleting old motion state. " << std::endl;
        delete tempMS;
    }
    
    mRB->setMotionState( motion );
    
    RegisterRigidBody( mRB );
    
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
    
    /*
     if( mMass != 0 )
     {
     setActivationState( DISABLE_DEACTIVATION );
     }
     */
    
    /*{
     std::string pname = mOSGToBullet->getName() + "_cr.osg";
     osg::ref_ptr< osgbBullet::PhysicsData > pd = new osgbBullet::PhysicsData();
     pd->_fileName = mOSGToBullet->getName();
     pd->_cr = record.get();
     pd->_body = mRB;
     //std::ostringstream ostr;
     //ostr << "id" << id++;
     //osg::ref_ptr< osgwTools::RefID > rid = new osgwTools::RefID( ostr.str() );
     osgDB::writeObjectFile( *pd.get(), pname );
     }*/
    
    //Comment this code out because it crashes with very large triangle meshes
    /*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*
     //This should probably go in CollisionShapes.cpp
     //The btInternalEdgeUtility helps to avoid or reduce artifacts
     //due to wrong collision normals caused by internal edges
     if( shapeType == TRIANGLE_MESH_SHAPE_PROXYTYPE )
     {
     //Avoid static_cast here, as it will always succeed
     btBvhTriangleMeshShape* bvhTriMeshShape =
     dynamic_cast< btBvhTriangleMeshShape* >( mRB->getCollisionShape() );
     if( bvhTriMeshShape )
     {
     btTriangleInfoMap* triangleInfoMap = new btTriangleInfoMap();
     btGenerateInternalEdgeInfo( bvhTriMeshShape, triangleInfoMap );
     }
     
     //Enable custom material callback
     mRB->setCollisionFlags(
     mRB->getCollisionFlags() |
     btCollisionObject::CF_CUSTOM_MATERIAL_CALLBACK );
     }
     *!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*!*/
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::CreateRigidBody(
    const std::string& lod,
    const std::string& motion,
    const std::string& mesh,
    const std::string& decimation )
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
        m_physicsMaterial = new Material( Material::SILLY_PUTTY );
    }
    else if( mesh == "Sphere" )
    {
        CustomShape( SPHERE_SHAPE_PROXYTYPE, overall, decimation );
        m_physicsMaterial = new Material( Material::SILLY_PUTTY );
    }
    else if( mesh == "Cylinder" )
    {
        CustomShape( CYLINDER_SHAPE_PROXYTYPE, overall, decimation );
        m_physicsMaterial = new Material( Material::SILLY_PUTTY );
    }
    else
    {
        if( motion == "Static" )
        {
            CustomShape( TRIANGLE_MESH_SHAPE_PROXYTYPE, overall, decimation );
            //This method runs very slowly
            //CustomShape( CONVEX_HULL_SHAPE_PROXYTYPE, overall, decimation );
            m_physicsMaterial = new Material( Material::CEMENT );
        }
        else
        {
            CustomShape( CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE, overall, decimation );
            m_physicsMaterial = new Material( Material::SILLY_PUTTY );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
Material* PhysicsRigidBody::GetSoundMaterial()
{
    return m_physicsMaterial;
}
////////////////////////////////////////////////////////////////////////////////
GhostController& PhysicsRigidBody::GetGhostController() const
{
    return *m_ghostController;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsRigidBody::EnableGhostControl( bool const& enable )
{
    if( m_ghostControl == enable )
    {
        return;
    }
    m_ghostControl = enable;

    btMotionState* ms( NULL );
    btGhostObject& ghostObject = m_ghostController->GetGhostObject();

    bool currentIdle = m_physicsSimulator.GetIdle();
    m_physicsSimulator.SetIdle( true );

    if( m_ghostControl )
    {
        //Add the ghost object and have static objects ignore it
        m_dynamicsWorld.addCollisionObject( &ghostObject );//,
            //btBroadphaseProxy::DefaultFilter,
            //btBroadphaseProxy::AllFilter ^ btBroadphaseProxy::StaticFilter );
        m_dynamicsWorld.addAction( m_ghostController );

        //Must move the ghost object to the current rigid body location
        ms = m_ghostController->GetMotionState();
        if( ms )
        {
            ms->getWorldTransform( ghostObject.getWorldTransform() );
        }

        //Remove the rigid body
        if( mRB )
        {
            //Clear forces and velocities
            mRB->clearForces();
            mRB->setAngularVelocity( btVector3( 0.0, 0.0, 0.0 ) );
            mRB->setLinearVelocity( btVector3( 0.0, 0.0, 0.0 ) );
            m_dynamicsWorld.removeRigidBody( mRB );
        }
    }
    else
    {
        //Add back the rigid body
        if( mRB )
        {
            m_dynamicsWorld.addRigidBody( mRB );
            //Need to do this to force rigid body to wake and test itself
            mRB->forceActivationState( ACTIVE_TAG );
            mRB->setDeactivationTime( 0.0 );

            //Update the rigid body and motion state transforms w/ the current
            //ghost object transform
            //We know the ghost transform is guaranteed to be free from collisions
            //because the ghost object has already tested and recovered from
            //penetrations
            ms = mRB->getMotionState();
            if( ms )
            {
                ms->getWorldTransform( mRB->getWorldTransform() );
            }
        }

        //Clean proxy from pairs for ghost object
        m_ghostController->Reset();

        //Remove the ghost object
        m_dynamicsWorld.removeCollisionObject( &ghostObject );
        m_dynamicsWorld.removeAction( m_ghostController );
    }

    m_physicsSimulator.SetIdle( currentIdle );
}
////////////////////////////////////////////////////////////////////////////////
