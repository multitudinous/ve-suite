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

#include "osgOQ/QueryExportDeclaration.h"
#include <osg/Node>
#include <osg/NodeVisitor>
#include <osg/Geode>
#include <osg/Geometry>

namespace osgOQ {


// Use this visitor to insert OcclusionQueryNodes (OQNs) in the
//   visited subgraph. OQNs may be nested, which could introduce
//   visual anomalies; see also OcclusionQueryFlatVisitor.
class OSGOQ_EXPORT OcclusionQueryNonFlatVisitor : public osg::NodeVisitor
{
public:
    OcclusionQueryNonFlatVisitor();
    virtual ~OcclusionQueryNonFlatVisitor();

    // Specify the vertex count threshold for performing occlusion
    //   query tests. Nodes in the scene graph whose total child geometry
    //   contains fewer vertices than the specified threshold will
    //   never be tested, just drawn. (In fact, they will br treated as
    //   potential occluders and rendered first in front-to-back order.)
    void setOccluderThreshold( int vertices );
    int getOccluderThreshold() const;

    virtual void apply( osg::Node& node );

protected:
    // When an OQR creates all OQNs and each OQN shares the same OQC,
    //   these methods are used to uniquely name all OQNs. Handy
    //   for debugging.
    std::string getNextOQNName();
    int getNameIdx() const { return _nameIdx; }

    osg::ref_ptr<osg::StateSet> _state;
    osg::ref_ptr<osg::StateSet> _debugState;

    unsigned int _nameIdx;

    int _occluderThreshold;
};

// Use this visitor to insert OcclusionQueryNodes (OQNs) in the
//   visited subgraph. Only one OQN will test any particular node
//   (no nesting). See also OcclusionQueryNonFlatVisitor.
class OSGOQ_EXPORT OcclusionQueryFlatVisitor : public osg::NodeVisitor
{
public:
    OcclusionQueryFlatVisitor();
    virtual ~OcclusionQueryFlatVisitor();

    // Specify the vertex count threshold for performing occlusion
    //   query tests. Nodes in the scene graph whose total child geometry
    //   contains fewer vertices than the specified threshold will
    //   never be tested, just drawn. (In fact, they will br treated as
    //   potential occluders and rendered first in front-to-back order.)
    void setOccluderThreshold( int vertices );
    int getOccluderThreshold() const;

    virtual void apply( osg::Group& group );
    virtual void apply( osg::Geode& geode );

protected:
    void addOQN( osg::Node& node );

    // When an OQR creates all OQNs and each OQN shares the same OQC,
    //   these methods are used to uniquely name all OQNs. Handy
    //   for debugging.
    std::string getNextOQNName();
    int getNameIdx() const { return _nameIdx; }

    osg::ref_ptr<osg::StateSet> _state;
    osg::ref_ptr<osg::StateSet> _debugState;

    unsigned int _nameIdx;

    int _occluderThreshold;
};

// OQNs derive from Group, and therefore can only perform tests on
//   subgraphs, not individual Drawables. To perform a query on a single
//   Drawable, the scene graph must be rearranged so that the target
//   Drawable has no sibling Drawables within its owning Geode(s); then
//   an OQN can be placed above the owning Geode(s) to effectively perform
//   a query only on the target Drawable. This NodeVisitor rearranges the
//   visited scene graph to allow query testing on large Drawables.
class OSGOQ_EXPORT PerDrawableQueryVisitor : public osg::NodeVisitor
{
public:
    PerDrawableQueryVisitor();
    virtual ~PerDrawableQueryVisitor();

    // After applying this visitor, new children are stored in a map but
    //   haven't yet been added back into the tree. Call rearrange() to
    //   effect this modification. (Called automatically in destructor.)
    int rearrange();

    // Specify the vertex count threshold for performing occlusion
    //   query tests. Nodes in the scene graph whose total child geometry
    //   contains fewer vertices than the specified threshold will
    //   never be tested, just drawn. (In fact, they will br treated as
    //   potential occluders and rendered first in front-to-back order.)
    void setOccluderThreshold( int vertices );
    int getOccluderThreshold() const;

    virtual void apply( osg::Geode& geode );

protected:
    osg::Geode* copyGeodeNoChildren( osg::Geode* src );

    unsigned int _occluderThreshold;
    unsigned int _count;

    typedef std::list< osg::ref_ptr<osg::Geode> > GeodeList;
    typedef std::list< osg::ref_ptr<osg::LOD> > LODList;
    typedef std::map< osg::ref_ptr<osg::Group>, GeodeList > GroupMap;
    typedef std::map< osg::ref_ptr<osg::LOD>, LODList > LODMap;
    GroupMap _newGroupChildren;
    LODMap _newLODChildren;
};


// Find all OQNs in the visited scene graph and set their visibility threshold.
class OSGOQ_EXPORT VisibilityThresholdVisitor : public osg::NodeVisitor
{
public:
    VisibilityThresholdVisitor( unsigned int threshold=500 )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _visThreshold( threshold ) {}
    virtual ~VisibilityThresholdVisitor() {}

    virtual void apply( osg::Node& node );

protected:
    unsigned int _visThreshold;
};

// Find all OQNs in the visited scene graph and set the number of frames
//   between queries.
class OSGOQ_EXPORT QueryFrameCountVisitor : public osg::NodeVisitor
{
public:
    QueryFrameCountVisitor( int count=5 )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _count( count ) {}
    virtual ~QueryFrameCountVisitor() {}

    virtual void apply( osg::Node& node );

protected:
    unsigned int _count;
};

// Find all OQNs in the visited scene graph and enable or disable queries..
class OSGOQ_EXPORT EnableQueryVisitor : public osg::NodeVisitor
{
public:
    EnableQueryVisitor( bool enable=true )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _enabled( enable ) {}
    virtual ~EnableQueryVisitor() {}

    virtual void apply( osg::Node& node );

protected:
    bool _enabled;
};

// Find all OQNs in the visited scene graph and enable or disable the
//   debug bounding volume display.
class OSGOQ_EXPORT DebugDisplayVisitor : public osg::NodeVisitor
{
public:
    DebugDisplayVisitor( bool debug=true )
      : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
        _debug( debug ) {}
    virtual ~DebugDisplayVisitor() {}

    virtual void apply( osg::Node& node );

protected:
    bool _debug;
};

// Remove all OQNs from the visited scene graph.
class OSGOQ_EXPORT RemoveOcclusionQueryVisitor : public osg::NodeVisitor
{
public:
    RemoveOcclusionQueryVisitor();
    virtual ~RemoveOcclusionQueryVisitor();

    virtual void apply( osg::Node& node );

protected:
};

// Gather statistics about OQN performance in the visited scene graph.
class OSGOQ_EXPORT StatisticsVisitor : public osg::NodeVisitor
{
public:
    StatisticsVisitor( osg::NodeVisitor::TraversalMode mode=osg::NodeVisitor::TRAVERSE_ACTIVE_CHILDREN );
    virtual ~StatisticsVisitor();

    virtual void apply( osg::Node& node );

    void reset();
    unsigned int getNumOQNs() const;
    unsigned int getNumPassed() const;

protected:
    unsigned int _numOQNs;
    unsigned int _numPassed;
};


}


#endif
