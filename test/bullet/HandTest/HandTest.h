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

#ifndef __HAND_TEST_H__
#define __HAND_TEST_H__


#include "DemoApplication.h"
#include "LinearMath/btAlignedObjectArray.h"
#include <string>

class btBroadphaseInterface;
class btCollisionShape;
class btOverlappingPairCache;
class btCollisionDispatcher;
class btConstraintSolver;
struct btCollisionAlgorithmCreateFunc;
class btDefaultCollisionConfiguration;


class HandTest : public DemoApplication
{

    //keep the collision shapes, for deletion/cleanup
    btAlignedObjectArray<btCollisionShape*>	m_collisionShapes;

    btBroadphaseInterface*	m_broadphase;

    btCollisionDispatcher*	m_dispatcher;

    btConstraintSolver*	m_solver;

    btDefaultCollisionConfiguration* m_collisionConfiguration;

public:

    std::string _fileName;

    HandTest()
    {
    }
    virtual ~HandTest()
    {
        exitPhysics();
    }
    void	initPhysics();
    void	exitPhysics();

    virtual void clientMoveAndDisplay();
    virtual void displayCallback();
    
	virtual void keyboardCallback( unsigned char key, int x, int y );
	virtual void specialKeyboard( int key, int x, int y );
	virtual void specialKeyboardUp( int key, int x, int y );

    static DemoApplication* Create()
    {
        HandTest* demo = new HandTest;
        demo->myinit();
        demo->initPhysics();
        return demo;
    }
};


#endif
