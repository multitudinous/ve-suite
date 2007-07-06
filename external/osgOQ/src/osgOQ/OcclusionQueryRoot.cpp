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

#include "osgOQ/OcclusionQueryRoot.h"
#include "osgOQ/OcclusionQueryVisitor.h"
#include "osgOQ/OcclusionQueryContext.h"
#include "osgOQ/OcclusionQueryNode.h"
#include <osg/NodeCallback>
#include <osg/Notify>
#include <osg/CopyOp>
#include <osg/Referenced>
#include <cassert>


using namespace osgOQ;


class OQRUpdateCB : public osg::NodeCallback
{
public:
	OQRUpdateCB() {}
	~OQRUpdateCB() {}

	virtual void operator()( osg::Node* node, osg::NodeVisitor* nv )
    { 
		traverse( node, nv );

		OcclusionQueryRoot* oqr = dynamic_cast< OcclusionQueryRoot* >( node );
		assert( oqr != NULL );
		oqr->update();
    }
};

OcclusionQueryRoot::OcclusionQueryRoot( OcclusionQueryContext* oqc )
  : _oqc( oqc ),
	_enabled( true ),
	_debug( false ),
	_debugVerbosity( 0 ),
	_prevDebugVerbosity( 0 ),
	_debugVerbosityFrames( -1 )
{
    setName( "OQRoot" );

	if (oqc == NULL)
        _oqc = new OcclusionQueryContext;

    setUpdateCallback( new OQRUpdateCB );
}

OcclusionQueryRoot::~OcclusionQueryRoot()
{
}

OcclusionQueryRoot::OcclusionQueryRoot( const OcclusionQueryRoot& oqr, const osg::CopyOp& copyop )
  : Group( oqr, copyop )
{
	// Note that we always "shallow copy" the OQC. If the caller requires
	//   a deep copy (the LHS needs its own OQC) the caller is responsible
	//   for replacing the OQC after the copy. There's no osg::CopyOp bit
	//   suitable for comtrolling how OQCs are copied.
	_oqc = oqr._oqc;

    // We could probably get away with assigning the same update callback to
    //   each OQR, so we could shallow-copy here for efficiency. However, this
    //   might not work if we enhance the update callback in the future to
    //   contain some per-OQR member variables, for example. So, be safe:
    //   Create a unique update callback for each OQR.
    setUpdateCallback( new OQRUpdateCB );

	_enabled = oqr._enabled;
	_debug = oqr._debug;
}

void
OcclusionQueryRoot::traverse( osg::NodeVisitor& nv )
{
	Group::traverse( nv );

    if ( !_oqc->getStatistics() ||
		(nv.getVisitorType() != osg::NodeVisitor::CULL_VISITOR) )
		return;

	// We just completed the cull and we now need to display statistics.
	osg::notify( osg::INFO ) <<
		"Queries: " << _oqc->getNumQueries() <<
		"  Passed: " << _oqc->getNumPassed() <<
		"  Failed: " << _oqc->getNumFailed() << std::endl;
	_oqc->clearStatistics();
}

bool
OcclusionQueryRoot::addChild( osg::Node* child )
{
	if (!Group::addChild( child ))
		return false;
	processNewChild( child );
	return true;
}

bool
OcclusionQueryRoot::insertChild( unsigned int idx, osg::Node* child )
{
	if (!Group::insertChild( idx, child ))
		return false;
	processNewChild( child );
	return true;
}

bool
OcclusionQueryRoot::setChild( unsigned int idx, osg::Node* child )
{
	if (!Group::setChild( idx, child ))
		return false;
	processNewChild( child );
	return true;
}

void
OcclusionQueryRoot::processNewChild( osg::Node* child )
{
	osgOQ::OcclusionQueryNode* oqn = dynamic_cast<osgOQ::OcclusionQueryNode*>( child );
	if (oqn != NULL)
		// The child is an OQN. We've already processed this subtree.
		// Without this return, we could infinite loop as the OQV
		//   calls replaceChild() to insert an OQN between the OQR
		//   (us) and our first child.
		return;

    // Adding OQNs could invalidate our "child" param, because it could
    //   insert a new OQN between the child and us. Get the index of
    //   "child" to avoid that issue.
    unsigned int idx = getChildIndex( child );

	if (_oqc->getNonFlatPlacement())
	{
		osgOQ::OcclusionQueryNonFlatVisitor oqv( _oqc.get() );
		child->accept( oqv );
	}
	else
	{
		osgOQ::OcclusionQueryFlatVisitor oqv( _oqc.get() );
		child->accept( oqv );
	}

    // Need to make sure query geometry is up-to-date initially.
    UpdateQueryGeometryVisitor uqgv;
    getChild( idx )->accept( uqgv );
}



class EnableQueryVisitor : public osg::NodeVisitor
{
public:
	EnableQueryVisitor( bool enable=true )
	  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
		_enabled( enable ) {}
	virtual ~EnableQueryVisitor() {}

    virtual void apply( osg::Node& node )
	{
		osgOQ::OcclusionQueryNode* oqn = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
		if (oqn != NULL)
			oqn->setQueriesEnabled( _enabled );
        traverse( node );
	}

protected:
	bool _enabled;
};

void
OcclusionQueryRoot::setQueriesEnabled( bool enable )
{
	if (_enabled == enable)
		return;
	_enabled = enable;

	EnableQueryVisitor eqv( _enabled );
	accept( eqv );
}


class DebugDisplayVisitor : public osg::NodeVisitor
{
public:
	DebugDisplayVisitor( bool debug=true )
	  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
		_debug( debug ) {}
	virtual ~DebugDisplayVisitor() {}

    virtual void apply( osg::Node& node )
	{
		osgOQ::OcclusionQueryNode* oqn = dynamic_cast<osgOQ::OcclusionQueryNode*>( &node );
		if (oqn != NULL)
			oqn->setDebugDisplay( _debug );
        traverse( node );
	}

protected:
	bool _debug;
};

void
OcclusionQueryRoot::setDebugDisplay( bool debug )
{
	if (_debug == debug)
		return;
	_debug = debug;

	DebugDisplayVisitor ddv( _debug );
	accept( ddv );
}


void
OcclusionQueryRoot::setDebugVerbosity( int level, int frames )
{
	_prevDebugVerbosity = _debugVerbosity;
	_debugVerbosity = level;
	if (_debugVerbosity > 1)
		_debugVerbosity = 1;
	if (_debugVerbosity < 0)
		_debugVerbosity = 0;

	if ( (_debugVerbosity != _prevDebugVerbosity) &&
		(frames > 0) )
	{
		_debugVerbosityFrames = frames;
		_oqc->setDebugVerbosity( _debugVerbosity );
	}
}

// Called once per frame by the OQRUpdateCB class during the
//   update traversal. Do the following:
// Check to see if we need to alter the debug verbosity. This is
//   done for debugging purposes to limit the amount of data
//   spewed out to the console.
// Check for a bounding box change. If so, launch a visitor to
//   look for bounding box changes in each OQN and update the
//   geometry used in the occlusion query test. (NOTE: This
//   DOES NOT handle changes to the bounding box that occur
//   after the update traversal and before the cull traversals.)
void
OcclusionQueryRoot::update()
{
    // This code is handy for debugging. It allows a developer to
    //   get gobs of information about osgOQ but only for a few frames.
	if ( _debugVerbosityFrames >= 0 )
	{
        if (_debugVerbosityFrames-- == 0)
        {
		    _debugVerbosity = _prevDebugVerbosity;
		    _oqc->setDebugVerbosity( _debugVerbosity );
        }
	}

    // If bound is dirty, launch visitor to update the query geometry.
    //   This will modify the scene graph but should be thread-safe
    //   because by definition we're in the update traversal. The only
    //   possible pitfall here is if the app modifies the scene graph
    //   after the update but before the cull/draw.
    if (!_boundingSphereComputed)
    {
        UpdateQueryGeometryVisitor uqgv;
        accept( uqgv );
    }
}

