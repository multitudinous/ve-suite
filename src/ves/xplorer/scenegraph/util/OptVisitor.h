//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __OPT_VISITOR_H__
#define __OPT_VISITOR_H__


#include <osg/NodeVisitor>
#include <osg/Geometry>
#include <iostream>


class OptVisitor : public osg::NodeVisitor
{
public:
    OptVisitor();
    virtual ~OptVisitor();

    //META_NodeVisitor(osgBullet,OptVisitor)

    virtual void apply( osg::Node& node );
    virtual void apply( osg::Geode& geode );

    bool changeDLtoVBO_;
    bool changeDynamicToStatic_;
    bool changeDAtoDEUI_;

    void dump( std::ostream& ostr );

protected:
    void processTriangles( const osg::DrawArrays& da, osg::VectorGLuint& indices );
    void processTriFan( const osg::DrawArrays& da, osg::VectorGLuint& indices );
    void processTriStrip( const osg::DrawArrays& da, osg::VectorGLuint& indices );

    unsigned int triangles_;
    unsigned int triFans_;
    unsigned int triStrips_;
    unsigned int newDEUIs_;
};


#endif
