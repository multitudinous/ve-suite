//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#include "osgOQ/OcclusionQueryNode.h"
#include "osgOQ/QueryGeometry.h"
#include "osgOQ/QueryState.h"
#include "osgOQ/OptionLoader.h"
#include <OpenThreads/ScopedLock>
#include <osg/Notify>
#include <osg/CopyOp>
#include <osg/Vec3>
#include <osg/MatrixTransform>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/BoundingBox>
#include <osg/BoundingSphere>
#include <osg/Referenced>
#include <osg/ComputeBoundsVisitor>
#include <osgUtil/CullVisitor>
#include <osg/Version>
#include <map>
#include <vector>
#include <cassert>


namespace osgOQ
{


OcclusionQueryNode::OcclusionQueryNode()
  : _enabled( true ),
    _visThreshold( 500 ),
    _queryFrameCount( 5 ),
    _debugBB( false )
{
    setDataVariance( osg::Object::DYNAMIC );

    // Override defaults if specified in the config file using the OptionLoader singleton.
    int clampMe;
    if ( OptionLoader::instance()->getOption( "VisibilityThreshold", clampMe ) )
        _visThreshold = (clampMe < 0 ) ? 0 : static_cast<unsigned int>( clampMe );
    if ( OptionLoader::instance()->getOption( "QueryFrameCount", clampMe ) )
        _queryFrameCount = (clampMe < 1) ? 1 : static_cast<unsigned int>( clampMe );
    OptionLoader::instance()->getOption( "DebugBoundingVolume", _debugBB );

    // OQN has two Geode member variables, one for doing the
    //   query and one for rendering the debug geometry.
    //   Create and initialize them.
    createSupportNodes();
}

OcclusionQueryNode::~OcclusionQueryNode()
{
}

OcclusionQueryNode::OcclusionQueryNode( const OcclusionQueryNode& oqn, const osg::CopyOp& copyop )
  : Group( oqn, copyop )
{
    _enabled = oqn._enabled;
    _debugBB = oqn._debugBB;

    // Regardless of shallow or deep, create unique support nodes.
    createSupportNodes();
}

// Currently, retrieves last frame's query results during current frame's cull traversal.
// In the future, should retrive last frame's results immediately after last
//   frame's buffer swap.
void
OcclusionQueryNode::traverse( osg::NodeVisitor& nv )
{
    if ( !_enabled || (nv.getVisitorType() != osg::NodeVisitor::CULL_VISITOR) )
    {
        osg::Group::traverse( nv );
        return;
    }

    osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>( &nv );
    osg::Camera* camera = cv->getRenderStage()->getCamera();


    // In the future, we could hold a reference directly to the QueryDrawable
    //   to avoid the dynamic_cast.
    QueryGeometry* qg = dynamic_cast< QueryGeometry* >( _queryGeode->getDrawable( 0 ) );
    if (qg == NULL)
    {
        osg::notify( osg::FATAL ) <<
            "osgOQ: OcclusionQueryNode: No QueryGeometry." << std::endl;
        return;
    }

    // If the distance to the bounding sphere shell is positive, retrieve
    //   the results. Others (we're inside the BS shell) we are considered
    //   to have passed and don't need to retrieve the query.
    const osg::BoundingSphere& bs = getBound();
    float distance = cv->getDistanceToEyePoint( bs._center, false )  - bs._radius;
    _passed = ( distance <= 0.f );
    if (!_passed)
    {
        int result = qg->getNumPixels( camera );
        _passed = ( (unsigned int)(result) > _visThreshold );
    }

    if (_passed)
        // We're visible. Traverse our children
        osg::Group::traverse( nv );

    // Submit a new query only if sufficient frames have elapsed.
    bool issueQuery;
    {
        const int curFrame = nv.getTraversalNumber();

        OpenThreads::ScopedLock<OpenThreads::Mutex> lock( _frameCountMutex );
        int& lastQueryFrame = _frameCountMap[ camera ];
        if ( issueQuery = (curFrame - lastQueryFrame >= _queryFrameCount) )
            lastQueryFrame = curFrame;
    }
    if (issueQuery)
        _queryGeode->accept( nv );

    if (_debugBB)
        // If requested, display the debug geometry
        _debugGeode->accept( nv );

    osg::notify( osg::DEBUG_INFO ) <<
        "oagOQ: OQN::traverse: OQN: " << getName() <<
        ", Cam: " << camera <<
        ", Passed: " << _passed << std::endl;
}

osg::BoundingSphere
OcclusionQueryNode::computeBound() const
{
    {
        // Need to make this routine thread-safe. Typically called by the update
        //   Visitor, or just after the update traversal, but could be called by
        //   an application thread or by a non-osgViewer application.
        OpenThreads::ScopedLock<OpenThreads::Mutex> lock( _computeBoundMutex )  ;

        // This is the logical place to put this code, but the method is const. Cast
        //   away constness to compute the bounding box and modify the query geometry.
        osgOQ::OcclusionQueryNode* nonConstThis = const_cast<osgOQ::OcclusionQueryNode*>( this );


        osg::ComputeBoundsVisitor cbv;
        nonConstThis->accept( cbv );
        osg::BoundingBox bb = cbv.getBoundingBox();

        osg::ref_ptr<osg::Vec3Array> v = new osg::Vec3Array;
        v->resize( 8 );
        (*v)[0] = osg::Vec3( bb._min.x(), bb._min.y(), bb._min.z() );
        (*v)[1] = osg::Vec3( bb._max.x(), bb._min.y(), bb._min.z() );
        (*v)[2] = osg::Vec3( bb._max.x(), bb._min.y(), bb._max.z() );
        (*v)[3] = osg::Vec3( bb._min.x(), bb._min.y(), bb._max.z() );
        (*v)[4] = osg::Vec3( bb._max.x(), bb._max.y(), bb._min.z() );
        (*v)[5] = osg::Vec3( bb._min.x(), bb._max.y(), bb._min.z() );
        (*v)[6] = osg::Vec3( bb._min.x(), bb._max.y(), bb._max.z() );
        (*v)[7] = osg::Vec3( bb._max.x(), bb._max.y(), bb._max.z() );

        osg::Geometry* geom = dynamic_cast< osg::Geometry* >( nonConstThis->_queryGeode->getDrawable( 0 ) );
        geom->setVertexArray( v.get() );

        geom = dynamic_cast< osg::Geometry* >( nonConstThis->_debugGeode->getDrawable( 0 ) );
        geom->setVertexArray( v.get() );
    }

    return Group::computeBound();
}


// Should only be called outside of cull/draw. No thread issues.
void
OcclusionQueryNode::setQueriesEnabled( bool enable )
{
    _enabled = enable;
}

// Should only be called outside of cull/draw. No thread issues.
void
OcclusionQueryNode::setDebugDisplay( bool debug )
{
    _debugBB = debug;
}
bool
OcclusionQueryNode::getDebugDisplay() const
{
    return _debugBB;
}



void
OcclusionQueryNode::setQueryStateSets( osg::StateSet* ss, osg::StateSet* ssDebug )
{
    if (!_queryGeode.valid() || !_debugGeode.valid())
    {
        osg::notify( osg::WARN ) << "osgOQ: OcclusionQueryNode:: Invalid support node(s)." << std::endl;
        return;
    }

    _queryGeode->setStateSet( ss );
    _debugGeode->setStateSet( ssDebug );
}

bool
OcclusionQueryNode::getPassed() const
{
    return _passed;
}


void
OcclusionQueryNode::createSupportNodes()
{
    GLushort indices[] = { 0, 1, 2, 3,  4, 5, 6, 7,
        0, 3, 6, 5,  2, 1, 4, 7,
        5, 4, 1, 0,  2, 7, 6, 3 };

    {
        // Add the test geometry Geode
        _queryGeode = new osg::Geode;
        _queryGeode->setName( "OQTest" );
        _queryGeode->setDataVariance( osg::Object::DYNAMIC );

        osg::ref_ptr< QueryGeometry > geom = new QueryGeometry( getName() );
        geom->setDataVariance( osg::Object::DYNAMIC );
        geom->addPrimitiveSet( new osg::DrawElementsUShort(
                    osg::PrimitiveSet::QUADS, 24, indices ) );

        _queryGeode->addDrawable( geom.get() );
    }

    {
        // Add a Geode that is a visual representation of the
        //   test geometry for debugging purposes
        _debugGeode = new osg::Geode;
        _debugGeode->setName( "Debug" );
        _debugGeode->setDataVariance( osg::Object::DYNAMIC );

        osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;
        geom->setDataVariance( osg::Object::DYNAMIC );

        osg::ref_ptr<osg::Vec4Array> ca = new osg::Vec4Array;
        ca->push_back( osg::Vec4( 1.f, 1.f, 1.f, 1.f ) );
        geom->setColorArray( ca.get() );
        geom->setColorBinding( osg::Geometry::BIND_OVERALL );

        geom->addPrimitiveSet( new osg::DrawElementsUShort(
                    osg::PrimitiveSet::QUADS, 24, indices ) );

        _debugGeode->addDrawable( geom.get() );
    }

    // Creste state sets. Note that the osgOQ visitors (which place OQNs throughout
    //   the scene graph) create a single instance of these StateSets shared
    //   between all OQNs for efficiency.
    setQueryStateSets( initOQState(), initOQDebugState() );
}


}
