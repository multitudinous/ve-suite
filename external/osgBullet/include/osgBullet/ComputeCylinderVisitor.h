/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#ifndef OSGBULLET_COMPUTECYLINDERVISITOR
#define OSGBULLET_COMPUTECYLINDERVISITOR    1

#include <osg/NodeVisitor>

#include <osgBullet/BoundingCylinder.h>

namespace osgBullet {

/* TBD Consider using OSG localtoworld method instead of keeping a matrix stack. */
class ComputeCylinderVisitor
    : public osg::NodeVisitor
{
public:

    ComputeCylinderVisitor( osg::NodeVisitor::TraversalMode traversalMode = TRAVERSE_ALL_CHILDREN );

    virtual void reset();


    virtual void setAxis( const osg::Vec3 a )
    {
        axis = a;
        axis.normalize();
        bc.setAxis( axis );
    }

    BoundingCylinder & getBoundingCylinder()
    {
        return( bc );
    }

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
    BoundingCylinder bc;
    osg::Vec3 axis;
};

}

#endif
