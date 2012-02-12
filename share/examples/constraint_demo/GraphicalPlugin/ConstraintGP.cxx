/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/communication/CommunicationHandler.h>

#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>

#include <ves/xplorer/scenegraph/util/MaterialPresent.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <osgDB/ReadFile>
#include <osgUtil/LineSegmentIntersector>
#include <osgUtil/Optimizer>

#include <osg/Depth>

#include <sstream>
#include <iostream>
#include <fstream>
#include <algorithm>

// pick this up from RTTScene for now
extern osg::ref_ptr<osg::Texture2D> RTTtex;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

#include "ConstraintGP.h"

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <osgbDynamics/MotionState.h>
#include <osgbDynamics/CreationRecord.h>
#include <osgbDynamics/RigidBody.h>
#include <osgbCollision/CollisionShapes.h>
#include <osgbCollision/RefBulletObject.h>
#include <osgbDynamics/GroundPlane.h>
#include <osgbCollision/GLDebugDrawer.h>
#include <osgbCollision/Utils.h>
#include <osgbInteraction/DragHandler.h>
#include <osgbInteraction/LaunchHandler.h>
#include <osgbInteraction/SaveRestoreHandler.h>

#include <btBulletDynamicsCommon.h>

#include <osgwTools/InsertRemove.h>
#include <osgwTools/FindNamedNode.h>
#include <osgwTools/GeometryOperation.h>
#include <osgwTools/GeometryModifier.h>
#include <osgwTools/Shapes.h>

using namespace Poco::Data;
using namespace ves::xplorer::scenegraph;
using namespace warrantytool;

//
// BEGIN WALL FIX
//

// The input model consists of two separate walls. However, the OSG scene
// graph for this is a single Geode with a single Geometry and a single
// QUADS PrimitiveSet. Our app needs to make this into two separate static
// collision shapes. One way to handle this situation would be to parse the
// geometry data and code directly to the Bullet API.
//
// Howver, for this example, I have instead chosen to sacrifice a little bit
// of rendering efficiency by fixing the scene graph, which will allow the
// example to use osgbDynamics::createRigidBody() to automatically generate
// the collision shapes. In order for this to work, the scene graph must contain
// two Geodes, each with its own Geometry and PrimitiveSet. This allows the
// osgBullet rigid body create code to simple create one rigid body for
// each branch of the graph.
//
// The FindGeomOp is a GeometryOperation that returns a reference to the last
// Geometry found, which is all we need to locate the Geometry in question in
// this branch of the scene graph. The FixWalls function makes a copy of this
// scene graph branch, then uses FindGeomOp to locate the Geometry, then
// modifies the PrimitiveSet on each to only reference the vertices needed for
// each wall segment.
//
// Obviously, this code is very model specific, and is not intended for
// re-use with other models. Therefore I have wrapped it with BEGIN WALL FIX
// and END WALL FIX.

/* \cond */
class FindGeomOp : public osgwTools::GeometryOperation
{
public:
    FindGeomOp() {}
    FindGeomOp( const FindGeomOp& rhs, const osg::CopyOp& copyOp=osg::CopyOp::SHALLOW_COPY ) 
        :
        osgwTools::GeometryOperation( rhs )
        {;}

    META_Object(osgBulletExamples,FindGeomOp);
    
    virtual osg::Geometry* operator()( osg::Geometry& geom )
    {
        _target = &geom;
        return( &geom );
    }
    
    osg::ref_ptr< osg::Geometry > _target;
};
/* \endcond */

#define METERS2FEET 3.28

// Filter out collisions between the gate and walls.
//
// Bullet collision filtering tutorial:
//   http://www.bulletphysics.com/mediawiki-1.5.8/index.php?title=Collision_Filtering
//
// Define filter groups
enum CollisionTypes {
    COL_GATE = 0x1 << 0,
    COL_WALL = 0x1 << 1,
    COL_DEFAULT = 0x1 << 2,
};
// Define filter masks
unsigned int gateCollidesWith( COL_DEFAULT );
unsigned int wallCollidesWith( COL_DEFAULT );
unsigned int defaultCollidesWith( COL_GATE | COL_WALL | COL_DEFAULT );

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
ConstraintGP::ConstraintGP()
    :
    PluginBase()
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "ConstraintPlugin";
}
////////////////////////////////////////////////////////////////////////////////
ConstraintGP::~ConstraintGP()
{
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    
    InitializeConstraintGraph();
}
////////////////////////////////////////////////////////////////////////////////
int ConstraintGP::InitializeConstraintGraph()
{    
    btDiscreteDynamicsWorld* bulletWorld = 
        dynamic_cast< btDiscreteDynamicsWorld* >( mPhysicsSimulator->GetDynamicsWorld() );
    osg::Group* root = new osg::Group();

    mDCS->addChild( root );
    
    osg::ref_ptr< osg::Node > rootModel = osgDB::readNodeFile( "Models/GateWall.ive" );
    if( !rootModel.valid() )
    {
        osg::notify( osg::FATAL ) << "hinge: Can't load data file \"GateWall.ive\"." << std::endl;
        return( 1 );
    }
    
    {
        util::MaterialPresent materialPresent( rootModel.get() );
    }
    
    
    // Scale to feet.
    {
        osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform(
            osg::Matrix::scale( osg::Vec3( METERS2FEET, METERS2FEET, METERS2FEET ) ) );
        mt->setDataVariance( osg::Object::STATIC );
        mt->addChild( rootModel.get() );
        
        osgUtil::Optimizer optimizer;
        optimizer.optimize( mt.get(), osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS );
    }
    
    root->addChild( rootModel.get() );
    
    // Get Node pointers and parent transforms for the wall and gate.
    // (Node names are taken from the osgWorks osgwnames utility.)
    osg::Matrix wallXform, gateXform;
    osg::Node* wallsNode = findNamedNode( rootModel.get(), "Walls", wallXform );
    osg::Node* gateNode = findNamedNode( rootModel.get(), "DOF_Gate", gateXform );
    if( ( wallsNode == NULL ) || ( gateNode == NULL ) )
    {
        std::cout << "Could not find the Gate or walls." << std::endl;
        return( 1 );
    }
    
    // BEGIN WALL FIX
    //
    // Unfortunately, the two walls come to us as a single Geode with
    // a single Geometry and single PrimitiveSet. Break that into two Geodes, a
    // left wall and a right wall, so we can make a collision shape for each.
    osg::Node* otherWall = fixWalls( wallsNode );
    wallsNode->getParent( 0 )->addChild( otherWall );
    otherWall->setName( "otherWall" );
    osg::Matrix otherWallXform = wallXform;
    //
    // END WALL FIX
    
        
    // Make Bullet rigid bodies and collision shapes for the gate...
    makeGate( bulletWorld, 0, gateNode, gateXform );
    // ...and the two walls.
    makeStaticObject( bulletWorld, wallsNode, wallXform );
    makeStaticObject( bulletWorld, otherWall, otherWallXform );
    
    // Add ground
    const osg::Vec4 plane( 0., 0., 1., 0. );
    root->addChild( osgbDynamics::generateGroundPlane( plane, bulletWorld,
                                                      NULL ) );//, COL_DEFAULT, defaultCollidesWith ) );
    
    
    // Create the hinge constraint.
    {
        // Pivot point and pivot axis are both in the gate's object space.
        // Note that the gate is COM-adjusted, so the pivot point must also be
        // in the gate's COM-adjusted object space.
        // TBD extract this from hinge data fine.
        const btVector3 btPivot( btVector3( -0.498f, -0.019f, 0.146f ) * METERS2FEET );
        
        btVector3 btAxisA( 0., 0., 1. );
        btHingeConstraint* hinge = new btHingeConstraint( *gateBody, btPivot, btAxisA );
        hinge->setLimit( -1.5f, 1.5f );
        bulletWorld->addConstraint( hinge, true );
    }

    std::cout << "Loaded all of the models and physics for the Gate." << std::endl;
    return( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintGP::PreFrameUpdate()
{    
    return;
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    std::cout << "ConstraintGP::SetCurrentCommand" << std::endl << std::flush;
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* ConstraintGP::fixWalls( osg::Node* wallsNode )
{
    osg::ref_ptr< osg::Node > otherWall;
    {
        osg::ref_ptr< osg::Group > srcTempGroup = new osg::Group;
        srcTempGroup->addChild( wallsNode );
        osg::ref_ptr< osg::Group > otherWallTempGroup = new osg::Group( *srcTempGroup,
                                                                       osg::CopyOp::DEEP_COPY_NODES | osg::CopyOp::DEEP_COPY_DRAWABLES | osg::CopyOp::DEEP_COPY_PRIMITIVES );
        otherWall = otherWallTempGroup->getChild( 0 );
    }
    
    unsigned int count;
    {
        osg::ref_ptr< FindGeomOp > findGeom = new FindGeomOp;
        osgwTools::GeometryModifier modifier( findGeom.get() );
        wallsNode->accept( modifier );
        
        osg::Geometry* geom = findGeom->_target.get();
        osg::DrawArrays* da = dynamic_cast< osg::DrawArrays* >( geom->getPrimitiveSet( 0 ) );
        count = da->getCount();
        da->setCount( count / 2 );
    }
    {
        osg::ref_ptr< FindGeomOp > findGeom = new FindGeomOp;
        osgwTools::GeometryModifier modifier( findGeom.get() );
        otherWall->accept( modifier );
        
        osg::Geometry* geom = findGeom->_target.get();
        osg::DrawArrays* da = dynamic_cast< osg::DrawArrays* >( geom->getPrimitiveSet( 0 ) );
        da->setFirst( count / 2 );
        da->setCount( count / 2 );
    }
    
    return( otherWall.release() );
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintGP::makeStaticObject( btDiscreteDynamicsWorld* bw, osg::Node* node, const osg::Matrix& )
{
    osg::ref_ptr< osgbDynamics::CreationRecord > cr = new osgbDynamics::CreationRecord;
    cr->_sceneGraph = node;
    cr->_shapeType = CONVEX_HULL_SHAPE_PROXYTYPE;
    cr->_mass = 0.f;
    btRigidBody* rb = osgbDynamics::createRigidBody( cr.get() );
    
    bw->addRigidBody( rb, 
                     COL_WALL, 
                     wallCollidesWith|btBroadphaseProxy::CharacterFilter );
}
////////////////////////////////////////////////////////////////////////////////
osg::Transform* ConstraintGP::makeGate( btDiscreteDynamicsWorld* bw, osgbInteraction::SaveRestoreHandler*, osg::Node* node, const osg::Matrix& m )
{
    osgwTools::AbsoluteModelTransform* amt = new osgwTools::AbsoluteModelTransform;
    amt->setDataVariance( osg::Object::DYNAMIC );
    osgwTools::insertAbove( node, amt );
    
    osg::ref_ptr< osgbDynamics::CreationRecord > cr = new osgbDynamics::CreationRecord;
    cr->_sceneGraph = amt;
    cr->_shapeType = CONVEX_HULL_SHAPE_PROXYTYPE;
    cr->setCenterOfMass( node->getBound().center() );
    cr->_parentTransform = m;
    cr->_mass = 1.f;
    cr->_restitution = .5f;
    btRigidBody* rb = osgbDynamics::createRigidBody( cr.get() );
    
    bw->addRigidBody( rb, COL_GATE, gateCollidesWith|btBroadphaseProxy::CharacterFilter );
    rb->setActivationState( DISABLE_DEACTIVATION );
    
    // Save RB in global, as AMT UserData (for DragHandler), and in SaveRestoreHandler.
    gateBody = rb;
    amt->setUserData( new osgbCollision::RefRigidBody( rb ) );
    
    return( amt );
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* ConstraintGP::findNamedNode( osg::Node* model, const std::string& name, osg::Matrix& xform )
{
    osgwTools::FindNamedNode fnn( name );
    model->accept( fnn );
    if( fnn._napl.empty() )
    {
        osg::notify( osg::FATAL ) << "hinge: Can't find node names \"" << name << "\"." << std::endl;
        return( NULL );
    }
    xform = osg::computeLocalToWorld( fnn._napl[ 0 ].second );
    return( fnn._napl[ 0 ].first );
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintGP::RemoveSelfFromSG()
{
    PluginBase::RemoveSelfFromSG();
}
////////////////////////////////////////////////////////////////////////////////
