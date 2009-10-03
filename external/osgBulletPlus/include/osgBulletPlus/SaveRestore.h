// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLETPLUS_SAVE_RESTORE_H__
#define __OSGBULLETPLUS_SAVE_RESTORE_H__ 1

#include "osgBulletPlus/Export.h"
#include "osgBulletPlus/DataLoader.h"
#include "osgBullet/PhysicsState.h"
#include <osg/Node>
#include <OpenThreads/Mutex>

#include <string>


namespace osgBulletPlus {


// Saves physics state. Creates two files:
//   <baseName>.osgb -- Physics state.
//   <baseName>_skel.osg -- Skeleton file.
OSGBULLETPLUS_EXPORT bool savePhysics( const std::string baseName, const osg::Node* root, const osgBullet::PhysicsState& ps );

// Restores physics state. Call restore() to start the restore process,
// then call status() to see when the load is complete. Note that status()
// returns RESTORE_COMPLETE just once, subsequent calls return RESTORE_IDLE
// until you call restore() again.
//
// Before calling restore(), 'root' should already be loaded with all
// scene graph data, but should not contain any AMT nodes. AMT nodes will
// be added asynchronously.
//
// This class uses a three-stage process to restore physics data:
// Stage 1: Load the .osgb file, containing a PhysicsState object.
// Stage 2: Load the skeleton file, and use a ParallelVisitor to
//   copy AMT nodes from the skeleton file into the scene graph.
// Stage 3: Match AMT RefIDs with RefIDs in PhysicsState, and
//   enable physics on AMT subgraphs accordingly.
//
// Stages 1 and 2 are executed with their own threads. Stage 3
// could have multiple threads, depending on the number of entries
// in the loaded PhysicsState. All threads are ran sequentially, with
// the status() function managing thread launch.
//
// AMT nodes will be added to the scene graph asynchronously. Rigid bodies
// will be added to the dynamics world asynchronously. For this reason, calling
// code needs to lock around access to the scene graph and physics simulation.
// RestorePhysics provides a lock mechanism for calling code to use.
// Example main rendering loop:
//   while() {
//     bool unlock( false );
//     if( saverestore.status() == RESTORE_IN_PROGRESS )
//     {
//       unlock = true;
//       restorephysics.lockSceneGraph( true );
//       restorephysics.lockPhysics( true );
//     }
//     Step the physics sim
//     viewer.frame();
//     if( unlock )
//     {
//       saverestore.lockSceneGraph( false );
//       saverestore.lockPhysics( false );
//     }
//   }
// Alternatively, the app might not run the physics sim at all during
// a restore, in which case the app doesn't need to lock the physics
// sim, but would still need to lock around scene graph access if rendering.
//
// Note that the locking mechanism is subject to change when osgBullet moves
// to a multithreaded physics sim algorithm.

class LoadSkeletonThread;
class PhysicsCreator;

class OSGBULLETPLUS_EXPORT RestorePhysics
{
public:
    RestorePhysics();
    ~RestorePhysics();

    // Input is a base file name, loaded scene graph (minus AMT nodes),
    // and the dynamics world. The base name is used to access the .osgb
    // file and skeleton file as follows:
    //   <baseName>.osgb
    //   <baseName>_skel.osg
    void restore( const std::string baseName, osg::Node* root, btDynamicsWorld* bw );

    // RESTORE_IDLE: No restore operation is in progress.
    // RESTORE_IN_PROGRESS: Restore has been launched and is running without error.
    // RESTORE_COMPLETE: Restore completed successfully. Next call to status() will return IDLE.
    // RESTORE_ERROR: An error occured and restoration has terminated. Next call to status() will return IDLE.
    enum RestoreStatus {
        RESTORE_IDLE,
        RESTORE_IN_PROGRESS,
        RESTORE_COMPLETE,
        RESTORE_ERROR
    };
    RestoreStatus status();

    // While status() returns RESTORE_IN_PROGRESS, calling app must
    // lock around scene graph accesses.
    void lockSceneGraph( bool lock ) const;
    osg::Node* getSceneGraph();

    // While status() returns RESTORE_IN_PROGRESS, calling app must
    // lock around physics sim accesses.
    void lockPhysics( bool lock ) const;
    btDynamicsWorld* getPhysics();

protected:
    osg::ref_ptr< osg::Node > _root;
    btDynamicsWorld* _bw;

    // Stage 1: Member variables for loading the .osgb PhysicsState file.
    LoadObjectThread* _lot;
    osg::ref_ptr< osgBullet::PhysicsState > _ps;

    // Stage 2: Member variables for loading the skeleton file.
    std::string _skelName;
    LoadSkeletonThread* _lst;

    // Stage 3: Member variables for enabling physics on subgraphs.
    osgBullet::PhysicsState::DataMap::const_iterator _dmIt;
    PhysicsCreator* _pc;

    mutable OpenThreads::Mutex _sgLock;
    mutable OpenThreads::Mutex _physicsLock;
};


// namespace osgBulletPlus
}

// __OSGBULLETPLUS_SAVE_RESTORE_H__ 
#endif
