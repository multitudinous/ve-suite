// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBBULLETPLUS_SAVE_RESTORE_H__
#define __OSGBBULLETPLUS_SAVE_RESTORE_H__ 1

#include "osgbBulletPlus/Export.h"
#include "osgbBulletPlus/DataLoader.h"
#include "osgbBullet/PhysicsState.h"
#include <osg/Node>
#include <OpenThreads/Mutex>

#include <string>


namespace osgbBulletPlus {


// Saves physics state. Creates two files:
//   <baseName>.osgb -- Physics state.
//   <baseName>_skel.osg -- Skeleton file.
OSGBBULLETPLUS_EXPORT bool savePhysics( const std::string baseName, const osg::Node* root, const osgbBullet::PhysicsState& ps );

// Restores physics state. Call restore() to start the restore process,
// then call status() to see when the load is complete. Note that status()
// returns RESTORE_COMPLETE just once, subsequent calls return RESTORE_IDLE
// until you call restore() again.
//
// Before calling restore(), 'root' should already be loaded with all
// scene graph data, but should not contain any AMT nodes. AMT nodes will
// be added asynchronously.
//
// This class uses a multi-stage process to restore physics data:
// Stage 1: Load the scene graph.
// Stage 2: Load the .osgb file, containing a PhysicsState object.
// Stage 3: Load the skeleton file, and use a ParallelVisitor to
//   copy AMT nodes from the skeleton file into the scene graph.
// Stage 4: Match AMT RefIDs with RefIDs in PhysicsState, and
//   enable physics on AMT subgraphs accordingly.
//
// Stages 1, 2, and 3 are executed with their own threads. Stage 4
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
// Note that the locking mechanism is subject to change when osgbBullet moves
// to a multithreaded physics sim algorithm.

class LoadSkeletonThread;
class PhysicsCreator;

class OSGBBULLETPLUS_EXPORT RestorePhysics
{
public:
    RestorePhysics();
    ~RestorePhysics();

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

    // Three interfaces for restoring data:
    // 1) Restore a scene graph only.
    // 2) Restore physics data only (scene graph is already loaded).
    // 3) Load a scene graph _and_ restore physics data for is.
    //
    // When restoring physics data, input is a base file name
    // and the dynamics world. The base name is used to access the
    // .osgb file and skeleton file as follows:
    //   <baseName>.osgb
    //   <baseName>_skel.osg
    RestoreStatus restore( const std::string& sceneGraphName, osg::Group* attachPoint );
    RestoreStatus restore( const std::string baseName, btDynamicsWorld* bw, osg::Node* root );
    RestoreStatus restore( const std::string baseName, btDynamicsWorld* bw, const std::string& sceneGraphName, osg::Group* attachPoint );

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
    std::string _sgName;
    osg::ref_ptr< osg::Group > _attachPoint;
    osg::ref_ptr< osg::Node > _root;
    btDynamicsWorld* _bw;
    std::string _osgbName;
    std::string _skelName;

    // Stage 1: Member variables for loading the scene graph.
    LoadNodeThread* _lnt;

    // Stage 2: Member variables for loading the .osgb PhysicsState file.
    LoadObjectThread* _lot;
    osg::ref_ptr< osgbBullet::PhysicsState > _ps;

    // Stage 3: Member variables for loading the skeleton file.
    LoadSkeletonThread* _lst;

    // Stage 4: Member variables for enabling physics on subgraphs.
    osgbBullet::PhysicsState::DataMap::const_iterator _dmIt;
    PhysicsCreator* _pc;

    mutable OpenThreads::Mutex _sgLock;
    mutable OpenThreads::Mutex _physicsLock;
};


// namespace osgbBulletPlus
}

// __OSGBBULLETPLUS_SAVE_RESTORE_H__ 
#endif
