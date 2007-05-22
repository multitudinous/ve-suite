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
#include <osg/Notify>
#include <osg/CopyOp>
#include <osg/Vec3>
#include <osg/MatrixTransform>
#include <osg/Switch>
#include <osg/Node>
#include <osg/Geode>
#include <osg/Drawable>
#include <osg/Geometry>
#include <osg/BoundingBox>
#include <osg/BoundingSphere>
#include <osg/Referenced>
#include <osgUtil/CullVisitor>
#include <osg/Version>
#include <map>
#include <vector>
#include <cassert>

#if (OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2)
// Version >1.2, use OSG's ComputeBoundsVisitor
#include <osg/ComputeBoundsVisitor>
#endif

namespace osgOQ
{

#if (OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2)
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

// TestResults -- stores info necessary for a Drawable
//   to perform an occlusion query. An OcclusionQueryNode's
//   second child is a Geode owning a single Drawable that
//   draws the occlusion test geometry. That Drawable stores
//   an instance of TestResult in its user data.
// Accessed during the update, cull, and draw traversals.
class TestResults : public osg::Referenced
{
public:
    TestResults( OcclusionQueryContext* oqc ) : _oqc( oqc ) {}

	// Reference back to the OcclusionQueryCOntext.
    osg::ref_ptr<OcclusionQueryContext> _oqc;

	// Needed for debug only: stores the name of the
	//   OcclusionQueryNode that owns the Drawable that
	//   owns this TestResult instance.
	std::string _oqnName;

	// Info contains context-specific information.
	//   The _id is the context-specific query identifier
	//   obtained from OpenGL. _active is true when a
	//   query gets issued and set to false when the
	//   result is retrieved.
    class Info
    {
    public:
        Info() : _id( 0 ), _active( false ), _numPixels( 0 ), _sortDistance( 0.f ) {}
        GLuint _id;
        mutable bool _active;
		GLint _numPixels;
		mutable float _sortDistance;
    };
    typedef std::map<unsigned int,Info> CtxInfoMap;
    mutable CtxInfoMap _ctxInfo;
};

// Executes during the draw traversal.
class DrawCB : public osg::Drawable::DrawCallback
{
#if (OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2)
	// After 1.2, param 1 changed from State to RenderInfo.
	// Warning: Version was still 1.2 on dev branch long after the 1.2 release,
	//   and finally got bumped to 1.9 in April 2007.
    void drawImplementation( osg::RenderInfo& renderInfo, const osg::Drawable* drawable ) const
    {
        unsigned int contextID = renderInfo.getState()->getContextID();
#else
    void drawImplementation( osg::State& state, const osg::Drawable* drawable ) const
    {
        unsigned int contextID = state.getContextID();
#endif
        const TestResults* tr = dynamic_cast<const TestResults*>( drawable->getUserData() );

        TestResults::CtxInfoMap::iterator it = tr->_ctxInfo.find( contextID );
        if (it == tr->_ctxInfo.end())
        {
            osg::notify( osg::FATAL ) << "osgOQ: DrawCB::drawImplementation: Can't find contextID (" << contextID << ")." << std::endl;
			return;
        }
        TestResults::Info& info = it->second;

		if (info._sortDistance <= 0.f )
		{
			// Camera (eye) is within the bounding sphere. Don't perform
			//   occlusion test in this case.
			if (tr->_oqc->getDebugVerbosity() > 0)
				osg::notify( osg::ALWAYS ) <<
					"DrawCB. OQNName: " << tr->_oqnName <<
					", Ctx: " << contextID <<
					", sortDistance: " << info._sortDistance << std::endl;
			return;
		}

        osg::Drawable::Extensions* ext = tr->_oqc->_extensions.get();

        ext->glBeginQuery( GL_SAMPLES_PASSED_ARB, info._id );
#if (OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2)
        drawable->drawImplementation( renderInfo );
#else
        drawable->drawImplementation( state );
#endif
        ext->glEndQuery( GL_SAMPLES_PASSED_ARB );
        info._active = true;

		if (tr->_oqc->getStatistics())
			tr->_oqc->incNumQueries();

		osg::notify( osg::DEBUG_INFO ) <<
			"DrawCB. OQNName: " << tr->_oqnName <<
			", Ctx: " << contextID <<
			", ID: " << info._id << std::endl;
#ifdef _DEBUG
        {
            GLenum err;
            if ((err = glGetError()) != GL_NO_ERROR)
                osg::notify( osg::FATAL ) <<
                "osgOQ: DrawCB: OpenGL error: " << err << "." << std::endl;
        }
#endif
    }
};

// Executes during the update traversal.
class UpdateCB : public osg::NodeCallback
{
public:
	UpdateCB() {}
	~UpdateCB() {}

	virtual void operator()( osg::Node* node, osg::NodeVisitor* nv )
    { 
		OcclusionQueryNode* oqn = dynamic_cast< OcclusionQueryNode* >( node );
		assert( oqn != NULL );

		int nc( oqn->getNumChildren() );
		bool bogus( (nc>3) || (nc<1) );

		if (!oqn->getQueriesEnabled() || bogus)
		{
			if (bogus)
				osg::notify( osg::WARN ) <<
					"OcclusionQueryNode: Invalid child count: " <<
					nc << "." << std::endl;

			// Occlusion Queries are disabled. Switch the primary child on,
			//   other children off, traverse, and return.
			if ( !oqn->getValue( 0 ) )
				oqn->setValue( 0, true );
			if ( oqn->getValue( 1 ) )
				oqn->setValue( 1, false );
			if ( oqn->getValue( 2 ) )
				oqn->setValue( 2, false );
		}
		else if (nc == 3)
			// Typical case. Not the first time through, and
			//   queries are enabled.
			oqn->switchChildren( nv->getTraversalNumber() );

		traverse( node, nv );

	    if (nc == 1)
			// First time through, only the attached subtree is a child.
			// Create additional children for OQ test and debug display.
			//   Must be done in update traversal (should never modify
			//   the scene graph outside the update traversal).
			oqn->createChildren();

		// If the bounding box changed, update vertex array for our
		//   occlusion query test and debug child Geodes.
		oqn->updateBB();
    }
};


OcclusionQueryNode::OcclusionQueryNode( OcclusionQueryContext* oqc )
  : _enabled( true ),
	_debug( false ),
	_oqc( oqc )
{
    if (oqc == NULL)
        _oqc = new OcclusionQueryContext;
	_debug = _oqc->getDebugDisplay();

	setName( _oqc->getNextOQNName() );

	setDataVariance( osg::Object::DYNAMIC );

	setUpdateCallback( new UpdateCB );
}

OcclusionQueryNode::~OcclusionQueryNode()
{
}

OcclusionQueryNode::OcclusionQueryNode( const OcclusionQueryNode& oqn, const osg::CopyOp& copyop )
  : Switch( oqn, copyop )
{
}

// Currently, retrieves last frame's query results during current frame's cull traversal.
// In the future, should retrive last frame's results immediately after last
//   frame's buffer swap.
void
OcclusionQueryNode::traverse( osg::NodeVisitor& nv )
{
    osg::Switch::traverse( nv );

	if ( !_enabled || (nv.getVisitorType() != osg::NodeVisitor::CULL_VISITOR) )
        return;

    int nc( getNumChildren() );
    if (nc != 3)
	{
        osg::notify( osg::FATAL ) <<
			"OcclusionQueryNode: traverse: getNumChildren() != 3." << std::endl;
		return;
	}

	osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>( &nv );
    unsigned int contextID = cv->getState()->getContextID();

    // Get an occlusion query ID for this context. This handles
	//   multiple contexts, and if we've already allocated an ID
	//   for the current context this is a no-op.
    initializeContext( contextID );

    // We should have 3 children, child 0 is the original subgraph,
    //   child 1 has our boundary query test geometry, and child 2
    //   visualizes the test geometry for debugging.
    // We should have tested it last frame, so get the test
    //   results and enable/disable child 0 accordingly.
    if (!_tr.valid())
	{
        osg::notify( osg::WARN ) <<
            "OcclusionQueryNode: TestResult not valid." << std::endl;
		return;
	}

    TestResults::CtxInfoMap::iterator it = _tr->_ctxInfo.find( contextID );
    if (it == _tr->_ctxInfo.end())
    {
        osg::notify( osg::FATAL ) << "OcclusionQueryNode::traverse: Can't find queryID." <<
            "contextID (" << contextID << ")." << std::endl;
		return;
    }

    TestResults::Info& info = it->second;

	// Record the distance from the camera (eye) to the bounding volume
	//   outer shell. If negative, camrea is inside the bounding sphere.
	//   No need to test in this case, just draw the actual geometry.
	const osg::BoundingSphere& bs = getBound();
	float distance = cv->getDistanceToEyePoint( bs._center, false );
	info._sortDistance = distance - bs._radius;

	bool passed( info._sortDistance <= 0.f );
	if (!passed)
	{
		if (!info._active)
		{
			// This test wasn't executed last frame. This is probably because
			//   a parent node failed the OQ test or this node is outside the
			//   view volume.
			// Do not obtain results from OpenGL.
			return;
		}

		osg::Drawable::Extensions* ext = _tr->_oqc->_extensions.get();
#if (OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2)
		ext->glGetQueryObjectiv( info._id, GL_QUERY_RESULT, &(info._numPixels) );
#else
		ext->glGetQueryObjectiv( info._id, GL_QUERY_RESULT_ARB, &(info._numPixels) );
#endif
		if (info._numPixels < 0)
			osg::notify( osg::WARN ) << "OcclusionQueryNode::traverse:" <<
				"glGetQueryObjectiv returned negative value (" << info._numPixels << ")." << std::endl;
		passed = (unsigned int)(info._numPixels) > _oqc->getVisibilityThreshold();
	}

	// Either retrieve last frame's results, or ignore it because the
	//   camera is inside the view. In either case, _active is now false.
    info._active = false;


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
			", ID: " << info._id <<
			", Dis: " << info._sortDistance <<
			"," << info._numPixels <<
			", Thresh: " << _oqc->getVisibilityThreshold() << std::endl;
}


// Called from traverse() during cull traversal.
void
OcclusionQueryNode::initializeContext( unsigned int contextID )
{
    const int nc( getNumChildren() );
    if ( _initialized.find( contextID ) != _initialized.end())
    {
        // Already initialized for this contextID.
        // Make sure there are 2 or 3 children.
        if (nc < 3)
            osg::notify( osg::WARN ) <<
                "OcclusionQueryNode: initializeContext(): getNumChildren() returns " <<
                nc << ", should be 3. contextID( " <<
                contextID << " )." << std::endl;
        return;
    }
    _initialized.insert( contextID );

    if (nc != 3)
    {
        osg::notify( osg::WARN ) <<
            "OcclusionQueryNode: initializeContext(): getNumChildren() returns " <<
            nc << ", should be 3." << std::endl;
        return;
    }

    // Create and attach TestResults.
    if (!_tr.valid())
	{
        _tr = new TestResults( _oqc.get() );
		_tr->_oqnName = getName();
	}
    TestResults::Info info;
    info._id = _oqc->getBufferIndex( contextID );
    _tr->_ctxInfo[ contextID ] = info;

	osg::Geode* geode = dynamic_cast< osg::Geode* >( getChild( 1 ) );
	osg::Geometry* geom = dynamic_cast< osg::Geometry* >( geode->getDrawable( 0 ) );
    geom->setUserData( _tr.get() );
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


// Executes during the update traversal, called by UpdateCB.
void
OcclusionQueryNode::switchChildren( const int traversalNum )
{
	if (getNumChildren() != 3)
		// Probably first time through and haven't initialized
		//   our occlusion query test and debug children yet.
		return;

	if (!_tr.valid())
		// Again, probably first time through.
		return;

	bool visible( false );
	TestResults::CtxInfoMap::iterator it = _tr->_ctxInfo.begin();
    while (it != _tr->_ctxInfo.end())
    {
	    TestResults::Info& info = it->second;
	    if (info._active)
		{
			if ( visible = ((unsigned int)(info._numPixels) > _oqc->getVisibilityThreshold()) )
				// It's visible in at least one context.
				//   Switch it on and look no further.
				break;
		}
		else
		{
			if ( visible = (info._sortDistance <= 0.f) )
				// Test wasn't active because eye was within the bounding volume.
				//   In this case, just draw it.
				break;
		}
		it++;
    }
	if ( getValue( 0 ) != visible )
	{
		if (_oqc->getDebugVerbosity() > 0)
		{
			osg::notify( osg::ALWAYS ) <<
				"OQN::SC: OQN: " << getName() <<
				", State: " << visible << std::endl;

			osg::NodePathList npl = getParentalNodePaths();
			osg::notify( osg::ALWAYS ) <<
				"  NPL size: " << npl.size() << std::endl;
			unsigned int idx0, idx1;
			for( idx0 = 0; idx0 < npl.size(); idx0++ )
			{
				osg::NodePath& np = npl[ idx0 ];
				for( idx1 = 0; idx1 < np.size(); idx1++ )
				{
					osg::Node* node = np[ idx1 ];
					osg::notify( osg::ALWAYS ) <<
						"    " << idx0 << " " << idx1 <<
						", Name: " << node->getName() <<
						", Class: " << node->className() << std::endl;
				}
			}
		}
		setValue( 0, visible );
	}

	if ( getValue( 1 ) != _enabled )
		setValue( 1, _enabled );
	if ( getValue( 2 ) != _debug )
		setValue( 2, _debug );
}

// Executes during the update traversal, called by UpdateCB.
void
OcclusionQueryNode::createChildren()
{
    if (getNumChildren() != 1)
    {
        osg::notify( osg::WARN ) <<
            "OcclusionQueryNode: createChildren(): getNumChildren() returns " <<
            getNumChildren() << ", should be 1." << std::endl;
        return;
    }

	GLushort indices[] = { 0, 1, 2, 3,  4, 5, 6, 7,
        0, 3, 6, 5,  2, 1, 4, 7,
        5, 4, 1, 0,  2, 7, 6, 3 };

    {
        // Add the test geometry Geode
        osg::ref_ptr<osg::Geode> geode = new osg::Geode;
        geode->setName( "OQTest" );
		geode->setDataVariance( osg::Object::DYNAMIC );
        geode->setStateSet( _oqc->getTestStateSet() );

        osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;
		geom->setDataVariance( osg::Object::DYNAMIC );
        // Must disable use of display lists. Forces OSG to call
		//   the draw callback each frame.
        geom->setUseDisplayList( false );
        geom->addPrimitiveSet( new osg::DrawElementsUShort(
                    osg::PrimitiveSet::QUADS, 24, indices ) );

        geode->addDrawable( geom.get() );
        // Add child and set it TRUE in the Switch.
        addChild( geode.get(), true );
        geom->setDrawCallback( new DrawCB );
    }

    {
        // Add a Geode that is a visual representation of the
        //   test geometry for debugging purposes
        osg::ref_ptr<osg::Geode> geode = new osg::Geode;
        geode->setName( "Debug" );
		geode->setDataVariance( osg::Object::DYNAMIC );
        geode->setStateSet( _oqc->getDebugStateSet() );

		osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;
		geom->setDataVariance( osg::Object::DYNAMIC );

        osg::ref_ptr<osg::Vec4Array> ca = new osg::Vec4Array;
        ca->push_back( osg::Vec4( 1.f, 1.f, 1.f, 1.f ) );
        geom->setColorArray( ca.get() );
        geom->setColorBinding( osg::Geometry::BIND_OVERALL );

        geom->addPrimitiveSet( new osg::DrawElementsUShort(
                    osg::PrimitiveSet::QUADS, 24, indices ) );

        geode->addDrawable( geom.get() );
        // Add child, on or off according to current _debug value.
        //   For debugging purposes, a NodeVisitor can come around
        //   later and turn it on.
        addChild( geode.get(), _debug );
    }
}

// Executes during the update traversal, called by UpdateCB.
void
OcclusionQueryNode::updateBB()
{
	if (_boundingSphereComputed || (getNumChildren() <= 1) )
		// Nothing to update, just return.
		return;

    // DO NOT get child 0 bounding sphere, instead use a visitor
    //   to get the bounding box. Child 0's bounding sphere is a
    //   sphere around the bounding box, which is less efficient.
#if (OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2)
	// Version <=1.2, use our own bounding box visitor.
	BoundingBoxVisitor bbv;
	getChild( 0 )->accept( bbv );
	osg::BoundingBox bb = bbv.getBound();
#else
	// Version >1.2, use OSG's ComputeBoundsVisitor.
	osg::ComputeBoundsVisitor cbv;
	getChild( 0 )->accept( cbv );
	osg::BoundingBox bb = cbv.getBoundingBox();
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

	osg::Geode* geode = dynamic_cast< osg::Geode* >( getChild( 1 ) );
	osg::Geometry* geom = dynamic_cast< osg::Geometry* >( geode->getDrawable( 0 ) );
	geom->setVertexArray( v.get() );

	geode = dynamic_cast< osg::Geode* >( getChild( 2 ) );
	geom = dynamic_cast< osg::Geometry* >( geode->getDrawable( 0 ) );
	geom->setVertexArray( v.get() );
}


}
