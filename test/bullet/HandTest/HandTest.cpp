/*
Bullet Continuous Collision Detection and Physics Library
Copyright (c) 2003-2006 Erwin Coumans  http://continuousphysics.com/Bullet/

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from the use of this software.
Permission is granted to anyone to use this software for any purpose, 
including commercial applications, and to alter it and redistribute it freely, 
subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
*/

//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

///scaling of the objects (0.1 = 20 centimeter boxes )
#define SCALING 0.1
#define START_POS_X -5
#define START_POS_Y -5
#define START_POS_Z -3

#include "HandTest.h"
#include "GlutStuff.h"
///btBulletDynamicsCommon.h is the main Bullet include file, contains most common include files.
#include "btBulletDynamicsCommon.h"
#include <stdio.h> //printf debugging
#include <iostream>

#include "BulletColladaConverter/ColladaConverter.h"



// Global. We use this to push the hand around with the keyboard.
btMotionState* handMotionState( NULL );
btRigidBody* handRigidBody( NULL );


void HandTest::clientMoveAndDisplay()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 

    float ms = getDeltaTimeMicroseconds();
    if (m_dynamicsWorld)
    {
        m_dynamicsWorld->stepSimulation(ms / 1000000.f);
        //optional but useful: debug drawing
        m_dynamicsWorld->debugDrawWorld();
    }
        
    renderme(); 

    glFlush();
    glutSwapBuffers();
}



void HandTest::displayCallback(void) {

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
    
    if (m_dynamicsWorld)
        m_dynamicsWorld->debugDrawWorld();

    renderme();

    glFlush();
    glutSwapBuffers();
}

void HandTest::keyboardCallback( unsigned char key, int x, int y )
{
    DemoApplication::keyboardCallback( key, x, y );
}
void HandTest::specialKeyboard( int key, int x, int y )
{
    unsigned int modifiers = glutGetModifiers();
    bool isCtrl( (modifiers & GLUT_ACTIVE_CTRL) != 0 );
    bool isShift( (modifiers & GLUT_ACTIVE_SHIFT) != 0 );
    if( !isCtrl && !isShift )
    {
        DemoApplication::specialKeyboard( key, x, y );
        return;
    }

    const float step( SCALING*.4 );
    btVector3 delta( 0., 0., 0. );
    switch( key )
    {
    case GLUT_KEY_UP:
    {
        if( isShift)
            delta[ 1 ] = step;
        else
            delta[ 2 ] = step;
        break;
    }
    case GLUT_KEY_DOWN:
    {
        if( isShift)
            delta[ 1 ] = -step;
        else
            delta[ 2 ] = -step;
        break;
    }
    case GLUT_KEY_LEFT:
    {
        delta[ 0 ] = step;
        break;
    }
    case GLUT_KEY_RIGHT:
    {
        delta[ 0 ] = -step;
        break;
    }
    default:
    {
        DemoApplication::specialKeyboard( key, x, y );
        return;
    }
    }

    btTransform dt;
    dt.setIdentity();
    dt.setOrigin( delta );

    btTransform m;
    handMotionState->getWorldTransform( m );
    handMotionState->setWorldTransform( m * dt );

    btCompoundShape* cs = dynamic_cast< btCompoundShape* >( handRigidBody->getCollisionShape() );
    if( cs )
        cs->recalculateLocalAabb();
}
void HandTest::specialKeyboardUp( int key, int x, int y )
{
    DemoApplication::specialKeyboardUp( key, x, y );
}




void HandTest::initPhysics()
{
    setTexturing(true);
    setShadows(true);

    setCameraDistance(btScalar(SCALING*75.));
    setCameraUp( btVector3( 0, 1, 0 ) );

    //collision configuration contains default setup for memory, collision setup
    m_collisionConfiguration = new btDefaultCollisionConfiguration();

    //use the default collision dispatcher. For parallel processing you can use a diffent dispatcher (see Extras/BulletMultiThreaded)
    m_dispatcher = new	btCollisionDispatcher(m_collisionConfiguration);

    m_broadphase = new btDbvtBroadphase();

    //the default constraint solver. For parallel processing you can use a different solver (see Extras/BulletMultiThreaded)
    btSequentialImpulseConstraintSolver* sol = new btSequentialImpulseConstraintSolver;
    m_solver = sol;

    m_dynamicsWorld = new btDiscreteDynamicsWorld(m_dispatcher,m_broadphase,m_solver,m_collisionConfiguration);

    m_dynamicsWorld->setGravity(btVector3(0,-10,0));

    std::cout << "Loading " << _fileName << std::endl;
    ColladaConverter* cc = new ColladaConverter( m_dynamicsWorld );
    bool result = cc->load( _fileName.c_str() );
    if( !result )
    {
        std::cerr << "Unable to load file " << _fileName << std::endl;
        exit( 1 );
    }

    // i don't know which shape belongs to which rigid body. One (the ground plane)
    // is static, the other (square donut) is dynamic.
    cc->getRigidBody( 1 )->setActivationState( DISABLE_DEACTIVATION );
    cc->getRigidBody( 2 )->setActivationState( DISABLE_DEACTIVATION );

    // Conditional compile to select a simple box instead of the loaded hand.
#define USE_SIMPLE_BOX
#ifdef USE_SIMPLE_BOX
    {
        btScalar mass(0.);

        btCollisionShape* newBox = new btBoxShape(btVector3(SCALING*7,SCALING*7,SCALING*7));
        btVector3 localInertia(0,0,0);

        btTransform transform;
        transform.setIdentity();
        transform.setOrigin( btVector3( 0, 5., -5. ) );
    
        handMotionState = new btDefaultMotionState( transform );
        btRigidBody::btRigidBodyConstructionInfo rbInfo(mass,handMotionState,newBox,localInertia);
        handRigidBody = new btRigidBody(rbInfo);
        handRigidBody->setActivationState( DISABLE_DEACTIVATION );

        //add the body to the dynamics world
        m_dynamicsWorld->addRigidBody( handRigidBody );
    }
#else
    // Index 0 is the hand object.
    handRigidBody = cc->getRigidBody( 0 );
    handMotionState = handRigidBody->getMotionState();
#endif

    handRigidBody->setCollisionFlags( handRigidBody->getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT );


    {
        btScalar mass(1.);
        const bool isDynamic = (mass != 0.f);

        btCollisionShape* newBox = new btBoxShape(btVector3(SCALING*9,SCALING*9,SCALING*9));
        btVector3 localInertia(0,0,0);
        if (isDynamic)
            newBox->calculateLocalInertia(mass,localInertia);

        btTransform transform;
        transform.setIdentity();
        transform.setOrigin( btVector3( 1.5, 0., -3. ) );
    
        btDefaultMotionState* myMotionState = new btDefaultMotionState(transform);
        btRigidBody::btRigidBodyConstructionInfo rbInfo(mass,myMotionState,newBox,localInertia);
        btRigidBody* body = new btRigidBody(rbInfo);
        body->setActivationState( DISABLE_DEACTIVATION );

        //add the body to the dynamics world
        m_dynamicsWorld->addRigidBody(body);
    }
    clientResetScene();
}
    

void HandTest::exitPhysics()
{
    //cleanup in the reverse order of creation/initialization

    //remove the rigidbodies from the dynamics world and delete them
    int i;
    for (i=m_dynamicsWorld->getNumCollisionObjects()-1; i>=0 ;i--)
    {
        btCollisionObject* obj = m_dynamicsWorld->getCollisionObjectArray()[i];
        btRigidBody* body = btRigidBody::upcast(obj);
        if (body && body->getMotionState())
        {
            delete body->getMotionState();
        }
        m_dynamicsWorld->removeCollisionObject( obj );
        delete obj;
    }

    //delete collision shapes
    for (int j=0;j<m_collisionShapes.size();j++)
    {
        btCollisionShape* shape = m_collisionShapes[j];
        delete shape;
    }

    delete m_dynamicsWorld;
    
    delete m_solver;
    
    delete m_broadphase;
    
    delete m_dispatcher;

    delete m_collisionConfiguration;

    
}
