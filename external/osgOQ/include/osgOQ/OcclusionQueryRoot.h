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

#ifndef OSGOQ_OCCLUSION_QUERY_ROOT_H
#define OSGOQ_OCCLUSION_QUERY_ROOT_H 1

#include "osgOQ/ExportDeclaration.h"
#include "osgOQ/OcclusionQueryContext.h"
#include <osg/CopyOp>
#include <osg/Switch>
#include <set>


namespace osgOQ {


class OSGOQ_EXPORT OcclusionQueryRoot : public osg::Group
{
public:
    OcclusionQueryRoot( OcclusionQueryContext* oqc=NULL );

protected:
    virtual ~OcclusionQueryRoot();

public:
    // Copy constructor using CopyOp to manage deep vs shallow copy.
    OcclusionQueryRoot( const OcclusionQueryRoot& oqr, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );

    META_Node( osgOQ, OcclusionQueryRoot );

	virtual void traverse( osg::NodeVisitor& nv );

    void setOQC( OcclusionQueryContext* oqc ) { _oqc = oqc; }
    OcclusionQueryContext& getOQC() const { return *(_oqc.get()); }

    virtual bool addChild( osg::Node *child );
    virtual bool insertChild( unsigned int idx, osg::Node* child );
    virtual bool setChild( unsigned int idx, osg::Node* child );

	void setQueriesEnabled( bool enable=true );
	bool getQueriesEnabled() const { return _enabled; }

	void setDebugDisplay( bool debug=true );
	bool getDebugDisplay() const { return _debug; }

	void setDebugVerbosity( int level, int frames=3 );
	void update();

protected:
	void processNewChild( osg::Node* child );

    osg::ref_ptr<OcclusionQueryContext> _oqc;

	bool _enabled;
	bool _debug;

	int _debugVerbosity;
	int _prevDebugVerbosity;
	int _debugVerbosityFrames;
};

}


#endif
