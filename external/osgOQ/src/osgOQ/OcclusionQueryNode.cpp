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
#include "osgOQ/OcclusionQueryContext.h"
#include "osgOQ/QueryGeometry.h"
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
#include <osgUtil/CullVisitor>
#include <osg/Version>
#include <map>
#include <vector>
#include <cassert>

// After the 1.2 release and leading up to the 2.0 release, OSG came out in a
//   series of "engineering releases" starting with 1.9.0, which changed the
//   OSG interface in an incompatible way from OSG v1.2.
#define POST_OSG_1_2 \
    ( (OSG_VERSION_MAJOR>1) || \
      ( (OSG_VERSION_MAJOR==1) && (OSG_VERSION_MINOR>2) ) )
#define PRE_OSG_1_9 \
    ( (OSG_VERSION_MAJOR<1) || \
      ( (OSG_VERSION_MAJOR==1) && (OSG_VERSION_MINOR<9) ) )

#if POST_OSG_1_2
// Version >1.2, use OSG's ComputeBoundsVisitor
#include <osg/ComputeBoundsVisitor>
#endif

namespace osgOQ
{

#if PRE_OSG_1_9
// Version <=1.2, use our own bounding box visitor.

// Runs during the update traversal.
class BoundingBoxVisitor : public osg::NodeVisitor
{
public:
    BoundingBoxVisitor()
        : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN )
    {
        osg::Matrix m;
        _m.push_back( m );
    }
    ~BoundingBoxVisitor() {}

    osg::BoundingBox getBound() { return _bb; }

    virtual void apply( osg::Node& node )
    {
        traverse( node );
    }
    virtual void apply( osg::MatrixTransform& mt )
    {
        osg::Matrix m = mt.getMatrix();
        pushTraversePop( m, (osg::Node&)mt );
    }

    virtual void apply( osg::Geode& geode )
    {
        unsigned int i;
        for( i = 0; i < geode.getNumDrawables(); i++ )
        {
            osg::Geometry* geom = dynamic_cast<osg::Geometry *>(geode.getDrawable(i));
            if( !geom )
                continue;

            osg::BoundingBox bb = geom->getBound();
            osg::Matrix c = _m.back();
            osg::Vec3 v0 = bb._min * c;
            osg::Vec3 v1 = bb._max * c;
            _bb.expandBy( v0 );
            _bb.expandBy( v1 );
        } 
    }

protected:
    void pushTraversePop( const osg::Matrix& m, osg::Node& node )
    {
        osg::Matrix c = _m.back();
        _m.push_back( m*c );
        traverse( node );
        _m.pop_back();
    }

    osg::BoundingBox _bb;
    std::vector<osg::Matrix> _m;
};
#endif


OcclusionQueryNode::OcclusionQueryNode( OcclusionQueryContext* oqc )
  : _enabled( true ),
	_debug( false ),
    _lastQueryFrame( 0 ),
	_oqc( oqc )
{
	setDataVariance( osg::Object::DYNAMIC );

    if (oqc == NULL)
        _oqc = new OcclusionQueryContext;
	_debug = _oqc->getDebugDisplay();

	setName( _oqc->getNextOQNName() );

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
    _debug = oqn._debug;
    _lastQueryFrame = oqn._lastQueryFrame;

    // Assume a shallow copy and share the OQC.
    _oqc = oqn._oqc;

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
    unsigned int contextID = cv->getState()->getContextID();


    // In the future, we could hold a reference directly to the QueryDrawable
    //   to avoid the dynamic_cast.
    QueryGeometry* qg = dynamic_cast< QueryGeometry* >( _queryGeode->getDrawable( 0 ) );
    if (qg == NULL)
    {
        osg::notify( osg::FATAL ) <<
            "OcclusionQueryNode: No QueryGeometry." << std::endl;
		return;
    }

    // If the distance to the bounding sphere shell is positive, retrieve
    //   the results. Others (we're inside the BS shell) we are considered
    //   to have passed and don't need to retrieve the query.
	const osg::BoundingSphere& bs = getBound();
	float distance = cv->getDistanceToEyePoint( bs._center, false )  - bs._radius;
	bool passed( distance <= 0.f );
    if (!passed)
    {
        int result = qg->retrieveQuery( contextID );
		passed = ( (unsigned int)(result) > _oqc->getVisibilityThreshold() );
	}

    if (passed)
        // We're visible. Traverse our children
        osg::Group::traverse( nv );

    // Submit a new query only if sufficient frames have elapsed.
    const int curFrame = nv.getTraversalNumber();
    if (curFrame - _lastQueryFrame >= _oqc->getQueryFrameCount())
    {
        _queryGeode->accept( nv );
        _lastQueryFrame = curFrame;
    }

    if (_debug)
        // If requested, display the debug geometry
        _debugGeode->accept( nv );

    if (_oqc->getStatistics())
	{
		if (passed)
			_oqc->incNumPassed();
		else
			_oqc->incNumFailed();
	}

	if (_oqc->getDebugVerbosity() > 0)
		osg::notify( osg::ALWAYS ) <<
			"OQN::traverse: OQN: " << getName() <<
			", Ctx: " << contextID <<
			", Passed: " << passed << std::endl;
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
	_debug = debug;
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
        _queryGeode->setStateSet( _oqc->getTestStateSet() );

        osg::ref_ptr< QueryGeometry > geom = new QueryGeometry( _oqc.get(), getName() );
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
        _debugGeode->setStateSet( _oqc->getDebugStateSet() );

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
}

// Executes during the update traversal, called UpdateQueryGeometryVisitor.
void
OcclusionQueryNode::updateQueryGeometry()
{
    // DO NOT get bounding sphere of children, instead use a visitor
    //   to get the bounding box. Children's bounding sphere is a
    //   sphere around the bounding box, which is less efficient.

	osg::Geometry* geom = dynamic_cast< osg::Geometry* >( _queryGeode->getDrawable( 0 ) );
    if ( ( geom->getVertexArray() != NULL) &&
        _boundingSphereComputed )
        // This node already has some query array data and the
        //   bounding sphere appears to be current. We don't
        //   need to change our query geometry. Just return.
        return;

#if POST_OSG_1_2
	// Version >1.2, use OSG's ComputeBoundsVisitor.
	osg::ComputeBoundsVisitor cbv;
    accept( cbv );
	osg::BoundingBox bb = cbv.getBoundingBox();
#else
	// Version <=1.2, use our own bounding box visitor.
	BoundingBoxVisitor bbv;
	accept( bbv );
	osg::BoundingBox bb = bbv.getBound();
#endif

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

	geom->setVertexArray( v.get() );

	geom = dynamic_cast< osg::Geometry* >( _debugGeode->getDrawable( 0 ) );
	geom->setVertexArray( v.get() );
}


}
