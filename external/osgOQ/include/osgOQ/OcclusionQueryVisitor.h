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

#ifndef OSGOQ_OCCLUSION_QUERY_VISITOR_H
#define OSGOQ_OCCLUSION_QUERY_VISITOR_H 1

#include "osgOQ/ExportDeclaration.h"
#include <osg/Node>
#include <osg/NodeVisitor>
#include <osg/Geode>
#include <osg/Geometry>

namespace osgOQ {

class OcclusionQueryContext;

class OSGOQ_EXPORT OcclusionQueryNonFlatVisitor : public osg::NodeVisitor
{
public:
    OcclusionQueryNonFlatVisitor( OcclusionQueryContext* oqc=NULL );
    virtual ~OcclusionQueryNonFlatVisitor();

    OcclusionQueryContext* getOQContext() const { return _oqc.get(); }

    virtual void apply( osg::Node& node );

protected:
    osg::ref_ptr<OcclusionQueryContext> _oqc;
};

class OSGOQ_EXPORT OcclusionQueryFlatVisitor : public osg::NodeVisitor
{
public:
    OcclusionQueryFlatVisitor( OcclusionQueryContext* oqc=NULL );
    virtual ~OcclusionQueryFlatVisitor();

    OcclusionQueryContext* getOQContext() const { return _oqc.get(); }

    virtual void apply( osg::Group& group );
    virtual void apply( osg::Geode& geode );

protected:
	void addOQN( osg::Node& node );

    osg::ref_ptr<OcclusionQueryContext> _oqc;
};


class OSGOQ_EXPORT UpdateQueryGeometryVisitor : public osg::NodeVisitor
{
public:
    UpdateQueryGeometryVisitor();
    virtual ~UpdateQueryGeometryVisitor();

    virtual void apply( osg::Node& node );
};


}


#endif
