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
#include <osg/Group>
#include <set>



namespace osgOQ {


// This Node performs occlusion query testing on its children.
//   You can use it directly to occlusion query test a portion
//   of your scene graph, or you can use it implicitly with an
//   OcclusionQueryRoot, which places OcclusionQueryNodes where
//   needed and acts as a master control.
class OSGOQ_EXPORT OcclusionQueryNode : public osg::Group
{
public:
    OcclusionQueryNode( OcclusionQueryContext* oqc=NULL );
protected:
    virtual ~OcclusionQueryNode();
public:
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

    void updateQueryGeometry();

protected:
	void createSupportNodes();

    osg::ref_ptr< osg::Geode > _queryGeode;
    osg::ref_ptr< osg::Geode > _debugGeode;

    bool _enabled;
	bool _debug;
    int _lastQueryFrame;
    osg::ref_ptr<OcclusionQueryContext> _oqc;
};

}


#endif
