/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
//#include <ves/xplorer/scenegraph/SceneManager.h>
//#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDiscreteDynamicsWorld.h>

// --- C/C++ Libraries --- //
#include <cassert>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity(
    std::string geomFile,
    ves::xplorer::scenegraph::DCS* parentDCS,
    bool isStream,
    std::string occlude,
    PhysicsSimulator* physicsSimulator )
    :
    mPhysicsFlag( false ),
    mTransparencyFlag( false ),
    mOpacity( 1.0 ),
    mPhysicsSimulator( physicsSimulator ),
    mPhysicsRigidBody( NULL ),
    mDCS( NULL ),
    mCADEntityHelper( NULL )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    mDCS = new ves::xplorer::scenegraph::DCS();
    mCADEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper();
    mCADEntityHelper->SetOcclusionCulling( occlude );
    mCADEntityHelper->LoadFile( geomFile.c_str(), isStream );
    mFileName.assign( geomFile );
    mDCS->SetName( "CADEntityDCS" );
    mDCS->addChild( mCADEntityHelper->GetNode() );
    parentDCS->AddChild( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity(
    osg::Node* node,
    ves::xplorer::scenegraph::DCS* parentDCS,
    PhysicsSimulator* physicsSimulator )
    :
    mPhysicsFlag( false ),
    mTransparencyFlag( false ),
    mOpacity( 1.0 ),
    mPhysicsSimulator( physicsSimulator ),
    mPhysicsRigidBody( NULL ),
    mDCS( NULL ),
    mCADEntityHelper( NULL )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    mDCS = new ves::xplorer::scenegraph::DCS();
    mCADEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper();
    if( !dynamic_cast< osg::Group* >( node ) )
    {
        osg::ref_ptr< osg::Group > tempGroup = new osg::Group();
        tempGroup->addChild( node );
        mCADEntityHelper->SetNode( tempGroup.get() );
    }
    else
    {
        mCADEntityHelper->SetNode( node );
    }
    mFileName.assign( "" );
    mDCS->SetName( "CADEntityDCS" );
    mDCS->addChild( mCADEntityHelper->GetNode() );
    parentDCS->AddChild( mDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity(
    ves::xplorer::scenegraph::CADEntityHelper* nodeToCopy,
    ves::xplorer::scenegraph::DCS* parentDCS,
    PhysicsSimulator* physicsSimulator )
    :
    mPhysicsFlag( false ),
    mTransparencyFlag( false ),
    mOpacity( 1.0 ),
    mPhysicsSimulator( physicsSimulator ),
    mPhysicsRigidBody( NULL ),
    mDCS( NULL ),
    mCADEntityHelper( NULL )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    mDCS = new ves::xplorer::scenegraph::DCS();
    mCADEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper( *nodeToCopy );

    if( mCADEntityHelper->GetNode() )
    {
        mFileName = mCADEntityHelper->GetNode()->getName();
    }
    
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
            if( mPhysicsRigidBody->GetbtRigidBody() )
            {
                mPhysicsSimulator->GetDynamicsWorld()->
                    removeRigidBody( mPhysicsRigidBody->GetbtRigidBody() );
            }
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
        //mDCS->SetPhysicsRigidBody( mPhysicsRigidBody );
    }
    
    return;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::CADEntityHelper* const CADEntity::GetNode() const
{
    return mCADEntityHelper;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* const CADEntity::GetDCS() const
{
    return mDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
PhysicsRigidBody* const CADEntity::GetPhysicsRigidBody()
{
    //can't say that this function is const because mPhysicsRigidBody can change
    InitPhysics();
    
    return mPhysicsRigidBody;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& CADEntity::GetFilename() const
{
    return mFileName;
}
////////////////////////////////////////////////////////////////////////////////
const float CADEntity::GetOpacityValue() const
{
    return mOpacity;
}
////////////////////////////////////////////////////////////////////////////////
const bool CADEntity::GetTransparentFlag() const
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
void CADEntity::Save()
{
    if( mPhysicsRigidBody )
    {
        // Add to PhysicsSata for save/restore.
        /*osg::ref_ptr< osgbBullet::PhysicsData > pd = new osgbBullet::PhysicsData;
        pd->_cr = converter.getOrCreateCreationRecord();
        pd->_body = rb;
        std::ostringstream ostr;
        ostr << "id" << id++;
        osg::ref_ptr< osgwTools::RefID > rid = new osgwTools::RefID( ostr.str() );
        ps.addPhysicsData( rid.get(), pd.get() );
        
        osgbBulletPlus::savePhysics( mFileName, mDCS->getParent( 0 ).get(), ps );*/
    }
}
////////////////////////////////////////////////////////////////////////////////
