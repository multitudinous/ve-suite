//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#include <osgViewer/Viewer>
#include <osgDB/ReadFile>
#include <osgDB/FileNameUtils>
#include <osgDB/FileUtils>
#include <osgGA/TrackballManipulator>
#include <osg/ShapeDrawable>
#include <osg/Geode>

#include <btBulletDynamicsCommon.h>
#include <BulletColladaConverter/ColladaConverter.h>


#include <osg/io_utils>
#include <iostream>

#include <osgBullet/MotionState.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/RigidBody.h>
#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/OSGToCollada.h>
#include <osgBullet/ColladaUtils.h>
#include <osgBullet/DebugBullet.h>
#include <osgBullet/Utils.h>


osgBullet::DebugBullet _debugBullet;

btDynamicsWorld* initPhysics()
{
    btDefaultCollisionConfiguration* collisionConfiguration = new btDefaultCollisionConfiguration();
    btCollisionDispatcher* dispatcher = new btCollisionDispatcher( collisionConfiguration );
    btConstraintSolver* solver = new btSequentialImpulseConstraintSolver;

    btVector3 worldAabbMin( -10000, -10000, -10000 );
    btVector3 worldAabbMax( 10000, 10000, 10000 );
    btBroadphaseInterface* inter = new btAxisSweep3( worldAabbMin, worldAabbMax, 1000 );

    btDynamicsWorld* dynamicsWorld = new btDiscreteDynamicsWorld( dispatcher, inter, solver, collisionConfiguration );

    dynamicsWorld->setGravity( btVector3( 0, 0, -9.8 ));

    return( dynamicsWorld );
}


class DaeLoader : public osgGA::GUIEventHandler
{
public:
    DaeLoader( osg::Transform* n, const std::string& baseName, btDynamicsWorld* dw )
      : _n( n ),
        _baseName( baseName ),
        _dw( dw )
    {
    }
    ~DaeLoader()
    {
    }

    bool handle( const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& )
    {
        switch( ea.getEventType() )
        {
            case osgGA::GUIEventAdapter::KEYUP:
            {
                int key( ea.getKey() );
                if( (key<'0') || (key>'9') )
                    return false;

                osg::Matrix initialTrans;
                osgBullet::AbsoluteModelTransform* amt = dynamic_cast< osgBullet::AbsoluteModelTransform* >( _n.get() );
                if( amt != NULL ) {
                    initialTrans = amt->getMatrix();
                    amt->setMatrix( osg::Matrix::identity() );
                }

                osgBullet::RigidBody* rb = dynamic_cast< osgBullet::RigidBody* >( _n->getUserData() );
                if( rb != NULL )
                {
                    osg::notify( osg::ALWAYS ) << "*** Removing rigid body" << std::endl;
                    btRigidBody* brb = rb->getRigidBody();
                    osgBullet::MotionState* motion = dynamic_cast< osgBullet::MotionState* >( brb->getMotionState() );
                    _debugBullet.remove( motion->getDebugTransform() );
                    _dw->removeRigidBody( brb );
                }

                char keych[2] = { key, 0 };
                std::string baseName( _baseName + std::string( (char*)&keych ) );
                std::string daeName( baseName + ".dae" );

                osg::NodePath np;
                np.push_back( new osg::MatrixTransform( initialTrans ) );
                if( !( osgBullet::loadDae( _n.get(), np, daeName, _dw, &_debugBullet ) ) )
                    return false;

                return true;
            }
            default:
            break;
        }
        return false;
    }

protected:
    osg::ref_ptr< osg::Transform > _n;
    std::string _baseName;
    btDynamicsWorld* _dw;

};



osg::MatrixTransform* createOSGBox( osg::Vec3 size )
{
    osg::Box* box = new osg::Box();
    box->setHalfLengths( size );

    osg::ShapeDrawable* shape = new osg::ShapeDrawable( box );
    shape->setColor( osg::Vec4( 1., 1., 1., 1. ) );
    osg::Geode* geode = new osg::Geode();
    geode->addDrawable( shape );

    osg::MatrixTransform* mt = new osg::MatrixTransform();
    mt->addChild( geode );

    return( mt );
}

osg::Node*
createGround( float w, float h, const osg::Vec3& center )
{
    osg::MatrixTransform* ground = createOSGBox( osg::Vec3( w, h, .1 ));

    osgBullet::OSGToCollada converter;
    converter.setSceneGraph( ground );
    converter.setShapeType( BOX_SHAPE_PROXYTYPE );
    converter.setMass( 0.f );
    converter.convert();

    btRigidBody* body = converter.getRigidBody();

    // OSGToCollada counts on FLATTEN_STATIC_TRANSFORMS to transform all
    // verts, but that doesn't work with ShapeDrawables, so we must
    // transform the box explicitly.
    btTransform transform; transform.setIdentity();
    transform.setOrigin( osgBullet::asBtVector3( center ) );
    body->getMotionState()->setWorldTransform( transform );
    body->setWorldTransform( transform );

    ground->setUserData( new osgBullet::RigidBody( body ) );

    return ground;
}

int main( int argc,
          char* argv[] )
{
    if( argc < 2 )
    {
        osg::notify( osg::FATAL ) << "Usage: colladaread <filename>" << std::endl;
        return 1;
    }
    std::string fileName( argv[ 1 ] );

    osg::ref_ptr< osg::Node > load = osgDB::readNodeFile( fileName );
    if( !load )
    {
        osg::notify( osg::FATAL ) << "Can't load input file(s)." << std::endl;
        return 1;
    }
    std::string baseName( osgDB::getNameLessExtension( fileName ) );
    osg::notify( osg::ALWAYS ) << "colladaread: Loaded model from " << fileName << std::endl;
    osg::notify( osg::ALWAYS ) << "Press the '0' key to load " << baseName << "0.dae (for example)." << std::endl;

    osg::ref_ptr< osgBullet::AbsoluteModelTransform > model =
        new osgBullet::AbsoluteModelTransform;
    model->addChild( load.get() );



    btDynamicsWorld* dynamicsWorld = initPhysics();

    osgViewer::Viewer viewer;

    osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator();
    viewer.setCameraManipulator( tb );
    viewer.addEventHandler( new DaeLoader( model.get(), baseName, dynamicsWorld ) );

    osg::ref_ptr<osg::Group> root = new osg::Group();
    root->addChild( model.get() );
    root->addChild( _debugBullet.getRoot() );


    // Compute a reasonable ground plane size based on the bounding sphere radius.
    float dim = load->getBound()._radius * 1.5;
    osg::Vec3 cen = load->getBound()._center;
    cen[ 2 ] -= dim;
    osg::ref_ptr< osg::Node > ground = createGround( dim, dim, cen );
    root->addChild( ground.get() );
    osgBullet::RigidBody* body = dynamic_cast< osgBullet::RigidBody* >( ground->getUserData() );
    dynamicsWorld->addRigidBody( body->getRigidBody() );


    double currSimTime;
    double prevSimTime = viewer.getFrameStamp()->getSimulationTime();

    viewer.setSceneData( root.get() );
    viewer.realize();

    while( !viewer.done())
    {
        currSimTime = viewer.getFrameStamp()->getSimulationTime();
        dynamicsWorld->stepSimulation( currSimTime - prevSimTime );
        prevSimTime = currSimTime;
        viewer.frame();
    }

    return( 0 );
}
