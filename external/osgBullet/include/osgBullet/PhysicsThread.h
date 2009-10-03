// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_PHYSICSTHREAD_H__
#define __OSGBULLET_PHYSICSTHREAD_H__ 1

#include <osgBullet/Export.h>
#include <OpenThreads/Thread>
#include <OpenThreads/Mutex>
#include <OpenThreads/Barrier>
#include <osg/Timer>
#include <iostream>
#include <stdlib.h>


// Forward declaraction
class btDynamicsWorld;

namespace osgBullet {


// Forward declaraction
class TripleBuffer;


class OSGBULLET_EXPORT PhysicsThread : public OpenThreads::Thread
{
public:
    PhysicsThread( btDynamicsWorld* bw, osgBullet::TripleBuffer* tb=NULL );
    ~PhysicsThread();

    // Call Thread::start() to launch the thread.
    virtual void run();

    // Cause the thread to exit. Call Thread::isRunning() to verify that the
    // thread has actually exited.
    void stopPhysics();


    // Temporarily pause the physics thread (to add a new rigid body,
    // or move a static body, for example). You could also call
    // stopPhysics then restart the thread, but this would incur the
    // overhead of stopping and starting a thread. Use pause to temporarily
    // halt the running thread.
    void pause( bool pause );

    // After telling the thread to pause, call isPaused() to ensure the
    // thread has reached the pause gate and is idle.
    bool isPaused() const;


    // Allows access to the Bullet dynamics world. If the calling code
    // intends to modify the dynamics world, it
    // is responsible for ensuring that the thread is not running (and
    // therefore not asynchronously modifying) the physics sim).
    btDynamicsWorld* getDynamicsWorld() const { return( _bw ); }

protected:
    bool isStopping() const;

    btDynamicsWorld* _bw;
    osg::Timer _timer;
    bool _stopped;
    int _pauseCount;

    osgBullet::TripleBuffer* _tb;

    mutable OpenThreads::Mutex _stopMutex;
    mutable OpenThreads::Mutex _pauseMutex;
    mutable OpenThreads::Barrier _pauseGate;
};


// namespace osgBullet
}

// __OSGBULLET_PHYSICSTHREAD_H__
#endif
