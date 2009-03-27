//
// Copyright (c) 2009 Skew Matrix Software LLC.
// All rights reserved.
//

#include <osgBullet/CollectVerticesVisitor.h>
#include <osg/Transform>
#include <osg/Geometry>
#include <osg/Geode>

#include <iostream>

using namespace osgBullet;
using namespace osg;


CollectVerticesVisitor::CollectVerticesVisitor( osg::NodeVisitor::TraversalMode traversalMode )
    : osg::NodeVisitor( traversalMode )
{
    verts_ = new osg::Vec3Array;
    reset();
}

void CollectVerticesVisitor::reset()
{
    stack_.clear();
    stack_.push_back( osg::Matrix::identity() );
    verts_->clear();
}

void CollectVerticesVisitor::apply( osg::Transform& transform )
{
    osg::Matrix matrix;
    matrix = stack_.back();
    transform.computeLocalToWorldMatrix( matrix, this );
    pushMatrix( matrix );

    traverse( transform );

    popMatrix();
}

void CollectVerticesVisitor::apply( osg::Geode& geode )
{
    unsigned int idx;
    for( idx = 0; idx < geode.getNumDrawables(); idx++ )
        applyDrawable( geode.getDrawable( idx ) );
}

void CollectVerticesVisitor::applyDrawable( osg::Drawable* drawable )
{
    osg::Geometry* geom = dynamic_cast< osg::Geometry* >( drawable );
    if( geom == NULL )
        return;

    const osg::Vec3Array* in = dynamic_cast< const osg::Vec3Array* >( geom->getVertexArray() );
    if( in == NULL )
    {
        osg::notify( osg::WARN ) << "CollectVerticesVisitor: Non-Vec3 vertex array encountered." << std::endl;
        return;
    }

    const osg::Matrix& m( stack_.back() );
    osg::Vec3Array::const_iterator iter;
    for( iter = in->begin(); iter != in->end(); iter++ )
    {
        verts_->push_back( *iter * m );
    }
}

