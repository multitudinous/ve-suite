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
#define NETL_DEMO

#include "HyperLabICEGP.h"
#ifdef NETL_DEMO
#include "RenderPrep.h"
#endif

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

#define CM2FEET 0.0328

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
HyperLabICEGP::HyperLabICEGP()
    :
    PluginBase()
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "HyperLabICEPlugin";
}
////////////////////////////////////////////////////////////////////////////////
HyperLabICEGP::~HyperLabICEGP()
{
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    
    InitializeLabModels();
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabICEGP::InitializeLabModels()
{    
    btDiscreteDynamicsWorld* bulletWorld = 
        dynamic_cast< btDiscreteDynamicsWorld* >( mPhysicsSimulator->GetDynamicsWorld() );
    osg::Group* root = new osg::Group();

    mDCS->addChild( root );
    
    osg::ref_ptr< osgDB::ReaderWriter::Options > options = new osgDB::ReaderWriter::Options;
    options->setOptionString( "dds_flip" );
    
    //osg::ref_ptr< osg::Group > root = new osg::Group;
    osg::ref_ptr< osg::Node > models = osgDB::readNodeFile( "Models/ControlRoom_v9.osg", options.get() );
    if( !( models.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
        return( 1 );
    }
    
    {
        // Main prep work for rendering.
#ifdef NETL_DEMO
        float textSize( 0.f );
        bool parallaxMap( false );
        RenderPrep renderPrep( models.get(), textSize, parallaxMap );
#endif
    }
    
    osg::ref_ptr< osg::Node > facility = osgDB::readNodeFile( "Models/HyperLab_Facility_v1.osg", options.get() );
    if( !( facility.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
        return( 1 );
    }
    
    /*osg::ref_ptr< osg::Node > collisions = osgDB::readNodeFile( "Models/HyperLab_Collision_v1.osg", options.get() );
    if( !( collisions.valid() ) )
    {
        osg::notify( osg::FATAL ) << "Can't open model file(s)." << std::endl;
        return( 1 );
    }*/

    // Scale to feet.
    {
        osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform(
            osg::Matrix::scale( osg::Vec3( CM2FEET, CM2FEET, CM2FEET ) ) );
        mt->setDataVariance( osg::Object::STATIC );
        mt->addChild( models.get() );
        mt->addChild( facility.get() );
        //mt->addChild( collisions.get() );
        
        osgUtil::Optimizer optimizer;
        optimizer.optimize( mt.get(), osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS );
    }
    
    root->addChild( models.get() );
    root->addChild( facility.get() );
    //root->addChild( collisions.get() );

    std::cout << "Loaded all of the models and physics for the Gate." << std::endl;
    return( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::PreFrameUpdate()
{    
    return;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    std::cout << "HyperLabICEGP::SetCurrentCommand" << std::endl << std::flush;
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabICEGP::RemoveSelfFromSG()
{
    PluginBase::RemoveSelfFromSG();
}
////////////////////////////////////////////////////////////////////////////////
