//
// Copyright (c) 2009 Skew Matrix Software LLC.
// All rights reserved.
//

#ifndef __OSGBULLET_COLLECT_VERTICES_VISITOR_H__
#define __OSGBULLET_COLLECT_VERTICES_VISITOR_H__ 1

#include <osg/NodeVisitor>
#include <osg/Array>
#include <osg/Version>

namespace osgBullet {

/* TBD Consider using OSG localtoworld method instead of keeping a matrix stack. */
class CollectVerticesVisitor : public osg::NodeVisitor
{
public:
    CollectVerticesVisitor( osg::NodeVisitor::TraversalMode traversalMode = osg::NodeVisitor::TRAVERSE_ALL_CHILDREN );

#if( ( OPENSCENEGRAPH_MAJOR_VERSION >= 2) && (OPENSCENEGRAPH_MINOR_VERSION >= 8) )
    META_NodeVisitor(osgBullet,CollectVerticesVisitor)
#endif

    virtual void reset();


    osg::Vec3Array* getVertices()
    {
        return( verts_.get() );
    }

    void apply( osg::Transform& transform );
    void apply( osg::Geode& geode );

    inline void pushMatrix( osg::Matrix & matrix )
    {
        stack_.push_back( matrix );
    }

    inline void popMatrix()
    {
        stack_.pop_back();
    }

    void applyDrawable( osg::Drawable* drawable );

protected:
    typedef std::vector< osg::Matrix >   MatrixStack;

    MatrixStack stack_;
    osg::ref_ptr< osg::Vec3Array > verts_;
};

}

#endif
