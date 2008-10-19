/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#ifndef OSGBULLET_COMPUTETRIMESHVISITOR
#define OSGBULLET_COMPUTETRIMESHVISITOR    1

#include <osg/Array>
#include <osg/NodeVisitor>

namespace osgBullet {

/* TBD Consider using OSG localtoworld method instead of keeping a matrix stack. */
class ComputeTriMeshVisitor
    : public osg::NodeVisitor
{
public:

    ComputeTriMeshVisitor( osg::NodeVisitor::TraversalMode traversalMode = TRAVERSE_ALL_CHILDREN );

    virtual void reset();


    osg::Vec3Array * getTriMesh()
    {
        return( mesh.get() );
    }

    void apply( osg::Node & node );


    void apply( osg::Transform & transform );


    void apply( osg::Geode & geode );


    inline void pushMatrix( osg::Matrix & matrix )
    {
        stack.push_back( matrix );
    }

    inline void popMatrix()
    {
        stack.pop_back();
    }

    void applyDrawable( osg::Drawable * drawable );

protected:
    typedef std::vector< osg::Matrix >   MatrixStack;

    MatrixStack stack;
    osg::ref_ptr< osg::Vec3Array > mesh;
};

}

#endif
