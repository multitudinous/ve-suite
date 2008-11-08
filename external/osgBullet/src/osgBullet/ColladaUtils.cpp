// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#include <btBulletDynamicsCommon.h>
#include <BulletColladaConverter/ColladaConverter.h>

#include <osgBullet/MotionState.h>
#include <osgBullet/RigidBody.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/ColladaUtils.h>
#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/DebugBullet.h>
#include <osgBullet/Utils.h>

#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osgDB/FileUtils>
#include <osg/Notify>
#include <osg/io_utils>

namespace osgBullet
{

btDynamicsWorld* initPhysics()
{
    btDefaultCollisionConfiguration * collisionConfiguration = new btDefaultCollisionConfiguration();
    btCollisionDispatcher * dispatcher = new btCollisionDispatcher( collisionConfiguration );
    btConstraintSolver * solver = new btSequentialImpulseConstraintSolver;

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface * inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );

    btDynamicsWorld * dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( btVector3( 0, 0, -9.8 ) );

    return( dynamicsWorld );
}

bool loadDae( osg::Transform* node, const osg::NodePath& np, const std::string& daeName,
    btDynamicsWorld* dw, osgBullet::DebugBullet* dbgB )
{
    osg::NodePath::const_iterator it;
    for( it=np.begin(); it!=np.end(); it++ )
        osg::notify( osg::ALWAYS ) << (*it)->className() << ", ";
    osg::notify( osg::ALWAYS ) << std::endl;

    std::string fullDaeName( osgDB::findDataFile( daeName ) );
    if( fullDaeName.empty() )
    {
        osg::notify( osg::FATAL ) << "Can't find DAE file: " << daeName << std::endl;
        osg::notify( osg::FATAL ) << "See scripts/mkdae.bat for info on creating DAE files using osgbpp." << std::endl;
        return false;
    }
    osg::notify( osg::ALWAYS ) << "Attempting to load DAE file: " << fullDaeName << std::endl;

    osg::BoundingSphere bs = node->getBound();
    osg::Vec3 com( bs._center );
    osg::notify( osg::ALWAYS ) << "COM: " << com << std::endl;

    osg::Matrix parentTrans = osg::computeLocalToWorld( np );

    osg::notify( osg::ALWAYS ) << "PreLoad" << std::endl;
    btDynamicsWorld* lw = initPhysics();
    ColladaConverter* cc = new ColladaConverter( lw );
    cc->load( fullDaeName.c_str() );
    btRigidBody* rb = cc->getRigidBody( 0 );
    cc->reset();
    lw->removeRigidBody( rb );
    delete cc;
    delete lw;
    osg::notify( osg::ALWAYS ) << "PostLoad" << std::endl;

    osgBullet::MotionState* motion = new osgBullet::MotionState;
    motion->setTransform( node );
    dw->addRigidBody( rb );
    osg::notify( osg::ALWAYS ) << "rb grav: " << osgBullet::asOsgVec3( rb->getGravity() ) << std::endl;

    // Add visual rep og Bullet Collision shape.
    if( dbgB != NULL )
    {
        osg::Node* visNode = osgBullet::osgNodeFromBtCollisionShape( rb->getCollisionShape() );
        if( visNode != NULL )
        {
            osgBullet::AbsoluteModelTransform* debugAMT = new osgBullet::AbsoluteModelTransform;
            debugAMT->addChild( visNode );
            motion->setDebugTransform( debugAMT );
            dbgB->addDynamic( debugAMT );
        }
    }

    motion->setCenterOfMass( com );
    motion->setParentTransform( parentTrans );
    rb->setMotionState( motion );

    node->setUserData( new osgBullet::RigidBody( rb ) );

    return true;
}

}
