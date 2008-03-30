/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/SceneNode.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Fog>
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#endif //_OSG

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h>

// --- C/C++ Libraries --- //
#include <cassert>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( std::string geomFile,
                      ves::xplorer::scenegraph::DCS* parentDCS,
                      bool isStream,
                      bool occlude,
                      PhysicsSimulator* physicsSimulator )
        :
        mPhysicsRigidBody( 0 ),
        mPhysicsFlag( false ),
        mTransparencyFlag( false ),
        mPhysicsSimulator( physicsSimulator ),
        mOpacity( 1.0f )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    mDCS = new ves::xplorer::scenegraph::DCS();
    mCADEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper();

    mCADEntityHelper->LoadFile( geomFile.c_str(), isStream, occlude );
    mFileName.assign( geomFile );
    mDCS->SetName( "CADEntityDCS" );
    mDCS->addChild( mCADEntityHelper->GetNode() );
    /*if( occlude )
    {
        mCADEntityHelper->AddOccluderNodes();
    }*/
    parentDCS->AddChild( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( osg::Node* node,
                      ves::xplorer::scenegraph::DCS* parentDCS,
                      PhysicsSimulator* physicsSimulator )
        :
        mPhysicsRigidBody( 0 ),
        mPhysicsFlag( false ),
        mTransparencyFlag( false ),
        mPhysicsSimulator( physicsSimulator ),
        mOpacity( 1.0f )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    mDCS = new ves::xplorer::scenegraph::DCS();
    mCADEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper();

    mCADEntityHelper->SetNode( node );
    mFileName.assign( "" );
    mDCS->SetName( "CADEntityDCS" );
    mDCS->addChild( mCADEntityHelper->GetNode() );
    parentDCS->AddChild( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( ves::xplorer::scenegraph::CADEntityHelper* nodeToCopy,
                      ves::xplorer::scenegraph::DCS* parentDCS,
                      PhysicsSimulator* physicsSimulator )
        :
        mPhysicsRigidBody( 0 ),
        mPhysicsFlag( false ),
        mTransparencyFlag( false ),
        mPhysicsSimulator( physicsSimulator ),
        mOpacity( 1.0f )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    mDCS = new ves::xplorer::scenegraph::DCS();
    mCADEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper( *nodeToCopy );

    mFileName = mCADEntityHelper->GetNode()->getName();
    mDCS->SetName( "CADEntityDCS" );
    mDCS->addChild( mCADEntityHelper->GetNode() );
    parentDCS->AddChild( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::~CADEntity()
{
    delete mCADEntityHelper;

    if( mPhysicsRigidBody )
    {
        if( mPhysicsSimulator )
        {
            mPhysicsSimulator->GetDynamicsWorld()->removeRigidBody( mPhysicsRigidBody );
        }
        delete mPhysicsRigidBody;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::InitPhysics()
{
    if( !mPhysicsRigidBody )
    {
        mPhysicsRigidBody = new ves::xplorer::scenegraph::PhysicsRigidBody(
                                 mDCS.get(), mPhysicsSimulator );
        mDCS->SetPhysicsRigidBody( mPhysicsRigidBody );
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::CADEntityHelper* CADEntity::GetNode()
{
    return mCADEntityHelper;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* CADEntity::GetDCS()
{
    return mDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::PhysicsRigidBody* CADEntity::GetPhysicsRigidBody()
{
    return mPhysicsRigidBody;
}
////////////////////////////////////////////////////////////////////////////////
std::string CADEntity::GetFilename()
{
    return mFileName;
}
////////////////////////////////////////////////////////////////////////////////
bool CADEntity::GetTransparentFlag()
{
    return mTransparencyFlag;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetTransparencyFlag( bool flag )
{
    mTransparencyFlag = flag;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetOpacityValue( float opacity )
{
    mOpacity = opacity;
}
////////////////////////////////////////////////////////////////////////////////
float CADEntity::GetOpacityValue()
{
    return mOpacity;
}
////////////////////////////////////////////////////////////////////////////////
