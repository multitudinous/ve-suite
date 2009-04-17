/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#include <osg/Transform>
#include <osg/Drawable>
#include <osg/Geode>
#include <osg/PrimitiveSet>
#include <osg/TriangleFunctor>

#include <osgBullet/ComputeTriMeshVisitor.h>

#include <iostream>

using namespace osgBullet;
using namespace osg;

struct ComputeTriMeshFunc
{
    ComputeTriMeshFunc()
    {
        vertices = new osg::Vec3Array;

        vertices->clear();
    }

    void inline operator()( const osg::Vec3 v1, const osg::Vec3 v2, const osg::Vec3 v3, bool _temp )
    {
        vertices->push_back( v1 );
        vertices->push_back( v2 );
        vertices->push_back( v3 );
    }

    osg::ref_ptr< osg::Vec3Array > vertices;
};

ComputeTriMeshVisitor::ComputeTriMeshVisitor( osg::NodeVisitor::TraversalMode traversalMode )
    : osg::NodeVisitor( traversalMode )
{
    stack.push_back( osg::Matrix::identity() );
    mesh = new osg::Vec3Array;
}

void ComputeTriMeshVisitor::reset()
{
    stack.clear();
    stack.push_back( osg::Matrix::identity() );
    mesh->clear();
}

void ComputeTriMeshVisitor::apply( osg::Node & node )
{
    traverse( node );
}

void ComputeTriMeshVisitor::apply( osg::Transform & transform )
{
    osg::Matrix matrix;

    matrix = stack.back();

    transform.computeLocalToWorldMatrix( matrix, this );

    pushMatrix( matrix );

    traverse( transform );

    popMatrix();
}

void ComputeTriMeshVisitor::apply( osg::Geode & geode )
{
    for( unsigned int i = 0; i < geode.getNumDrawables(); ++i )
    {
        applyDrawable( geode.getDrawable( i ) );
    }
}

void ComputeTriMeshVisitor::applyDrawable( osg::Drawable * drawable )
{
    osg::TriangleFunctor< ComputeTriMeshFunc > functor;
    drawable->accept( functor );

    const osg::Matrix& matrix = stack.back();
    for( osg::Vec3Array::iterator iter = functor.vertices->begin();
         iter != functor.vertices->end(); ++iter )
    {
        mesh->push_back( *iter * matrix );
    }
}

