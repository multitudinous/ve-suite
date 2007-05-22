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

#ifndef OSGOQ_OCCLUSION_QUERY_NODE_H
#define OSGOQ_OCCLUSION_QUERY_NODE_H 1

#include "osgOQ/ExportDeclaration.h"
#include "osgOQ/OcclusionQueryContext.h"
#include <osg/CopyOp>
#include <osg/Switch>
#include <set>


namespace osgOQ {


class TestResults;

// This class is for internal use by osgOQ, but is exported under
//   Visual Studio so that the dot OSG wrapper (osgdb_osgOQ) can
//   access it.
class OSGOQ_EXPORT OcclusionQueryNode : public osg::Switch
{
public:
    OcclusionQueryNode( OcclusionQueryContext* oqc=NULL );
    virtual ~OcclusionQueryNode();

    // Copy constructor using CopyOp to manage deep vs shallow copy.
    OcclusionQueryNode( const OcclusionQueryNode& oqn, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );

    META_Node( osgOQ, OcclusionQueryNode );

    virtual void traverse( osg::NodeVisitor& nv );

	void setQueriesEnabled( bool enable=true );
	bool getQueriesEnabled() const { return _enabled; }

	void setDebugDisplay( bool debug=true );
	bool getDebugDisplay() const { return _debug; }

    void setOQC( OcclusionQueryContext* oqc ) { _oqc = oqc; }
    OcclusionQueryContext& getOQC() const { return *(_oqc.get()); }

	void switchChildren( const int traversalNum );
	void createChildren();
	void updateBB();

protected:
    void initializeContext( unsigned int contextID );

    std::set<unsigned int> _initialized;

	bool _enabled;
	bool _debug;
    osg::ref_ptr<OcclusionQueryContext> _oqc;

	osg::ref_ptr<TestResults> _tr;
};

}


#endif
