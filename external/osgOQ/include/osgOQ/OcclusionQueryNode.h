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

#include "osgOQ/QueryExportDeclaration.h"
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
    OcclusionQueryNode();

    // Copy constructor using CopyOp to manage deep vs shallow copy.
    OcclusionQueryNode( const OcclusionQueryNode& oqn, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );

    META_Node( osgOQ, OcclusionQueryNode );

    virtual void traverse( osg::NodeVisitor& nv );

    virtual osg::BoundingSphere computeBound() const;

    // When disabled, OQN doesn't perform occlusion queries, and simply
    //   renders its children.
    void setQueriesEnabled( bool enable=true );
    bool getQueriesEnabled() const { return _enabled; }


    // Sets/gets the visibility threshold. If the test indicates that
    //   the number of visible pixels is less than the specified
    //   threshold, don't draw the actual geometry.
    void setVisibilityThreshold( unsigned int pixels ) { _visThreshold = pixels; }
    unsigned int getVisibilityThreshold() const { return _visThreshold; }

    // Specifies how many frames to wait before issuing another query.
    void setQueryFrameCount( int frames ) { _queryFrameCount = frames; }
    int getQueryFrameCount() const { return _queryFrameCount; }

    // Indicate whether or not the bounding box used in the occlusion query test
    //   should be rendered. Handy for debugging and development.
    // Should only be called outside of cull/draw. No thread issues.
    void setDebugDisplay( bool enable );
    bool getDebugDisplay() const;


    // Set the StateSets used by the OQN when rendering OQ geometry
    //   or debug bounding geometry.
    void setQueryStateSets( osg::StateSet* ss, osg::StateSet* ssDebug );

    // For statistics gathering, e.g., by a NodeVisitor.
    bool getPassed() const;

protected:
    virtual ~OcclusionQueryNode();

    void createSupportNodes();

    osg::ref_ptr< osg::Geode > _queryGeode;
    osg::ref_ptr< osg::Geode > _debugGeode;

    bool _enabled;

    // Tracks the last frame number that we performed a query.
    // User can set how many times  (See setQueryFrameCount).
    typedef std::map< osg::Camera*, int > FrameCountMap;
    FrameCountMap _frameCountMap;
    mutable OpenThreads::Mutex _frameCountMutex;

    // For statistics gathering
    bool _passed;

    // User-settable variables
    unsigned int _visThreshold;
    int _queryFrameCount;
    bool _debugBB;


    // Required to ensure that computeBound() is thread-safe.
    mutable OpenThreads::Mutex _computeBoundMutex;
};

}


#endif
