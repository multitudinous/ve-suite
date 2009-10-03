// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include "osgBulletPlus/SaveRestore.h"
#include "osgBulletPlus/DataLoader.h"
#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/FileUtils>
#include <osgTools/ParallelVisitor.h>
#include <osgTools/AbsoluteModelTransform.h>
#include <osgTools/InsertRemove.h>
#include <osgTools/RefID.h>


namespace osgBulletPlus {


bool
savePhysics( const std::string baseName, const osg::Node* root, const osgBullet::PhysicsState& ps )
{
    bool result;

    // Save physics state.
    const std::string osgbName( baseName + std::string( ".osgb" ) );
    result = osgDB::writeObjectFile( ps, osgbName );
    if( !result )
    {
        osg::notify( osg::FATAL ) << "savePhysics: Failes to write file \"" << osgbName << "\"." << std::endl;
        return false;
    }

    // Make copy for skeleton file.
    osg::ref_ptr< osg::Node > rootCopy;
    if( root->asGroup() )
        rootCopy = new osg::Group( *( root->asGroup() ),
            osg::CopyOp::DEEP_COPY_OBJECTS | osg::CopyOp::DEEP_COPY_NODES );
    else
        rootCopy = new osg::Node( *root,
            osg::CopyOp::DEEP_COPY_OBJECTS | osg::CopyOp::DEEP_COPY_NODES );
    if( rootCopy == NULL )
    {
        osg::notify( osg::FATAL ) << "savePhysics: Failed to copy scene graph." << std::endl;
        return false;
    }

    const std::string skelName( baseName + std::string( "_skel.osg.skeleton" ) );
    result = osgDB::writeNodeFile( *rootCopy, skelName );
    if( !result )
    {
        osg::notify( osg::FATAL ) << "savePhysics: Failes to write file \"" << skelName << "\"." << std::endl;
        return false;
    }

    return true;
}


RestorePhysics::RestorePhysics()
  : _lot( NULL ),
    _lst( NULL ),
    _pc( NULL )
{
}

RestorePhysics::~RestorePhysics()
{
}



// Stage 3 helper function.

// Visit a scene graph and look for a specific RefID.
// If found, enable physics for the subgraph using the
// specified PhysicsData.

// The node with the RefID must be an AMT and have only one child.
class EnablePhysicsForRefID : public osg::NodeVisitor
{
public:
    EnablePhysicsForRefID( osgTools::RefID* rid, osgBullet::PhysicsData* pd, RestorePhysics* rp )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _rid( rid ),
        _pd( pd ),
        _rp( rp )
    {}
    ~EnablePhysicsForRefID() {}

    virtual void apply( osg::Node& node )
    {
        osgTools::RefID* rid = dynamic_cast< osgTools::RefID* >( node.getUserData() );
        if( (rid == NULL) || (rid->str() != _rid->str()) )
        {
            traverse( node );
            return;
        }

        osgTools::AbsoluteModelTransform* parent = dynamic_cast< osgTools::AbsoluteModelTransform* >( &node );
        if( parent == NULL )
        {
            osg::notify( osg::FATAL ) << "EnablePhysicsForRefID: Node for enable physics is not an AMT." << std::endl;
            traverse( node );
            return;
        }
        if( parent->getNumChildren() != 1 )
        {
            osg::notify( osg::FATAL ) << "EnablePhysicsForRefID: Number of children should be 1, but is: " << parent->getNumChildren() << std::endl;
            traverse( node );
            return;
        }
        osg::Node* child = parent->getChild( 0 );


        // Found the AMT Node with the correct refID (parent) and its
        // single child node (child. Now enable physics for the AMT.

        // Copy the subgraph for use with OSGToCollada.
        osg::Group* asGrp = child->asGroup();
        osg::ref_ptr< osg::Node > sgCopy;
        if( asGrp != NULL )
            sgCopy = new osg::Group( *asGrp, osg::CopyOp::DEEP_COPY_ALL );
        else
            sgCopy = new osg::Node( *child, osg::CopyOp::DEEP_COPY_ALL );

        const osg::ref_ptr< osgBullet::CreationRecord > cr( _pd->_cr );
        osgBullet::OSGToCollada converter( *cr );
        converter.setSceneGraph( sgCopy.get() );

        converter.convert();

        btRigidBody* rb = converter.getRigidBody();
        osgBullet::MotionState* ms = new osgBullet::MotionState;
        ms->setCenterOfMass( cr->_com );
        ms->setScale( cr->_scale );


        // Lock while modifying the scene graph.
        _rp->lockSceneGraph( true );

        rb->setMotionState( ms );
        ms->setTransform( parent );
        ms->setWorldTransform( osgBullet::asBtTransform( _pd->_bodyWorldTransform ) );
        rb->setWorldTransform( osgBullet::asBtTransform( _pd->_bodyWorldTransform ) );
        rb->setLinearVelocity( osgBullet::asBtVector3( _pd->_linearVelocity ) );
        rb->setAngularVelocity( osgBullet::asBtVector3( _pd->_angularVelocity ) );

        // Lock while modifying the phsics sim.
        _rp->lockPhysics( true );
        _rp->getPhysics()->addRigidBody( rb );
        _rp->lockPhysics( false );

        _rp->lockSceneGraph( false );


        // Probably don't need to do this. Hard to imagine
        // a subgraph with physics enabled...?!?
        // traverse( node );
    }

protected:
    osg::ref_ptr< osgTools::RefID > _rid;
    osg::ref_ptr< osgBullet::PhysicsData > _pd;
    RestorePhysics* _rp;
};

// Stage 3 thread class
//
// Run the EnablePhysicsForRefID helper NodeVisitor in a thread
// to enable physics for a single subgraph.
class PhysicsCreator : public OpenThreads::Thread
{
public:
    PhysicsCreator( const std::string& idStr, osgBullet::PhysicsData* pd, RestorePhysics* rp )
      : _idStr( idStr ),
        _pd( pd ),
        _rp( rp )
    {
        osg::notify( osg::INFO ) << "new PhysicsCreator" << std::endl;
    }

    ~PhysicsCreator()
    {}

    void run()
    {
        // find RefIDs in scene graph and enable physics for each subgraph.
        osg::ref_ptr< osgTools::RefID > rid = new osgTools::RefID( _idStr );
        EnablePhysicsForRefID epr( rid.get(), _pd.get(), _rp );
        _rp->getSceneGraph()->accept( epr );
    }

    const std::string _idStr;
    osg::ref_ptr< osgBullet::PhysicsData > _pd;
    RestorePhysics* _rp;
};


// Stage 2 helper class.
//
// Given two nodes, one an AMT and one not, make a copy
// of the AMT and insert it above the other node.
//
// This is a ParallelVisitor callback used to sychronize
// two scene graphs. During a physics restore, 'nodeB' comes
// from the skeleton file, and 'nodeA' comes from the OSG
// scene graph.
struct AddAMTCallback : public osgTools::ParallelVisitor::ParallelVisitorCallback
{
    RestorePhysics* _rp;

    AddAMTCallback( RestorePhysics* rp )
      : _rp( rp )
    {}

    virtual bool operator()( osg::Node& nodeA, osg::Node& nodeB )
    {
        if( nodeB.className() == std::string( "AbsoluteModelTransform" ) )
        {
            osgTools::AbsoluteModelTransform* amtSkel = static_cast< osgTools::AbsoluteModelTransform* >( &nodeB );
            osg::ref_ptr< osgTools::AbsoluteModelTransform > amt = new osgTools::AbsoluteModelTransform(
                *amtSkel, osg::CopyOp::DEEP_COPY_OBJECTS );
            amt->setDataVariance( osg::Object::DYNAMIC );
            amt->removeChildren( 0, amt->getNumChildren() );

            // Must lock scene graph when we add the AMT to
            // aboid thread collisions with rendering thread.
            _rp->lockSceneGraph( true );
            osgTools::insertAbove( &nodeA, amt.get() );
            _rp->lockSceneGraph( false );
        }
        return( true );
    }
};

// Stage 2 thread.
//
// Load the skeleton file, then run a ParallelVisitor to
// copy AMT nodes from the skeleton file into the main scene graph.
class LoadSkeletonThread : public OpenThreads::Thread
{
public:
    LoadSkeletonThread( const std::string& fileName, RestorePhysics* rp )
      : _fileName( fileName ),
        _rp( rp )
    {
        osg::notify( osg::INFO ) << "new LoadSkeletonThread" << std::endl;
    }

    ~LoadSkeletonThread()
    {}

    void run()
    {
        // Load skeleton file
        osg::ref_ptr< osg::Node > skel = osgDB::readNodeFile( _fileName );
        if( skel == NULL )
        {
            osg::notify( osg::FATAL ) << "RestorePhysics: Failes to read file \"" << _fileName << "\"." << std::endl;
            return;
        }

        // Add AMTs from skeleton file to root scene graph.
        // The code is designed to work correctly if the scene graph
        // being processed in the RestorePhysics object is equal to
        // the skeleton file, just with SMTs stripped.
        osgTools::ParallelVisitor pv( _rp->getSceneGraph(), skel.get() );
        pv.setCallback( new AddAMTCallback( _rp ) );
        pv.compare();
    }

    const std::string& _fileName;

    RestorePhysics* _rp;
};


// Start a physics restore. Calling code must repeatedly call status()
// from the main thread in order for restore to progress.
//
// Input baseName must match baseName passed to save.
// Input root must be a fully loaded scene graph, but no AMT nodes
//   (they will be added as part of the restore).
//
// TBD. In the future, we could enhance this code to load the
// root in a thread, rather than pass it in. Need to spec this. For
// example, does this code just load a single file, or does it load
// multiple files? If multiple, how does it arrange them in a scene
// graph? For now, the calling app can load the data in a thread
// using osgBulletPlus::loadNodeThread, and just pass root to us.
void
RestorePhysics::restore( const std::string baseName, osg::Node* root, btDynamicsWorld* bw )
{
    osg::notify( osg::INFO ) << "Starting restore" << std::endl;

    _root = root;
    _bw = bw;

    // Before proceeding with the restore, make sure the files exist.
    std::string osgbName( baseName + std::string( ".osgb" ) );
    if( !osgDB::fileExists( osgbName ) )
    {
        osg::notify( osg::FATAL ) << "RestorePhysics: Unable to find file \"" << osgbName << "\"." << std::endl;
        return;
    }
    _skelName = std::string( baseName + std::string( "_skel.osg" ) );
    if( !osgDB::fileExists( _skelName ) )
    {
        osg::notify( osg::FATAL ) << "RestorePhysics: Unable to finbd file \"" << _skelName << "\"." << std::endl;
        return;
    }

    // Start by loading the physics state object. See the status()
    //   function for additional steps in the restore process.
    _lot = new LoadObjectThread( osgbName );
    _lot->start();
}

// Return the current restore status.
//
// If a restore is in progress, watch for the completion of
// the stage threads, and launch new threads as needed.
RestorePhysics::RestoreStatus
RestorePhysics::status()
{
    if(_lot != NULL)
    {
        // Stage 1: We're loading the .osgb file.
        osg::notify( osg::INFO ) << "Restore stage 1" << std::endl;

        if( _lot->isRunning() )
            // We're still in stage 1. Nothing to do.
            return( RESTORE_IN_PROGRESS );

        // Stage 1 just completed. Get the PhysicsState.
        _ps = dynamic_cast< osgBullet::PhysicsState* >( _lot->_object.get() );
        delete _lot;
        _lot = NULL;

        if( _ps == NULL )
        {
            osg::notify( osg::FATAL ) << "RestorePhysics: Failed to load physics data." << std::endl;
            return( RESTORE_ERROR );
        }

        // Move on to stage 2: Create and run LoadSkeletonThread.
        _lst = new LoadSkeletonThread( _skelName, this );
        _lst->start();

        return( RESTORE_IN_PROGRESS );
    }
    else if( _lst != NULL )
    {
        // Stage 2: We're loading the skeleton file and syncing AMT nodes
        //   with the scene graph.
        osg::notify( osg::INFO ) << "Restore stage 2" << std::endl;

        if( _lst->isRunning() )
            // We're still in stage 2. Nothing to do.
            return( RESTORE_IN_PROGRESS );

        // Stage 2 is complete. Now that we've synched the skeleton
        // file with the scene graph, we have no need for the loaded
        // skeleton file. It gets deleted when we delete the _lst.
        delete _lst;
        _lst = NULL;

        // Move on to stage 3. Iterate over the PhysicsState data map.
        // For each entry, enable physics for the subgraph tagged with the
        // map entry's RefID.
        _dmIt = _ps->getDataMap().begin();
        if( _dmIt == _ps->getDataMap().end() )
        {
            // Physics state is empty!
            osg::notify( osg::WARN ) << "RestorePhysics: Loaded PhysicsState, but it is empty." << std::endl;
            return( RESTORE_COMPLETE );
        }

        // Create and run a PhysicsCreator for the first object on the list.
        _pc = new PhysicsCreator( _dmIt->first, _dmIt->second.get(), this );
        _dmIt++;
        _pc->start();

        return( RESTORE_IN_PROGRESS );
    }
    else if( _pc != NULL )
    {
        // Stage 3:
        // For each object listed in PhysicsState, create collision shapes
        //   and rigid bodies, and add them to the Bullet dynamics world.
        osg::notify( osg::INFO ) << "Restore stage 3" << std::endl;

        if( _pc->isRunning() )
            // We're still in stage 3 and still enabling physics on
            //   some subgraph. Nothing to do.
            return( RESTORE_IN_PROGRESS );

        // PhysicsCreator has exited; presumably we just added physics
        // to a node in the scene graph.
        delete _pc;
        _pc = NULL;

        if( _dmIt == _ps->getDataMap().end() )
        {
            // We've added physics for all objects listed in PhysicsState. We're done.
            osg::notify( osg::INFO ) << "Restore complete" << std::endl;
            return( RESTORE_COMPLETE );
        }

        // Load physics for the next object.
        _pc = new PhysicsCreator( _dmIt->first, _dmIt->second.get(), this );
        _dmIt++;
        _pc->start();

        return( RESTORE_IN_PROGRESS );
    }

    return( RESTORE_IDLE );
}

void
RestorePhysics::lockSceneGraph( bool lock ) const
{
    if( lock )
        _sgLock.lock();
    else
        _sgLock.unlock();
}

osg::Node*
RestorePhysics::getSceneGraph()
{
    return( _root.get() );
}

void
RestorePhysics::lockPhysics( bool lock ) const
{
    if( lock )
        _physicsLock.lock();
    else
        _physicsLock.unlock();
}

btDynamicsWorld*
RestorePhysics::getPhysics()
{
    return( _bw );
}


// namespace osgBulletPlus
}
