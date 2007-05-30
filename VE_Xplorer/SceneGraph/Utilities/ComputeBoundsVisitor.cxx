#include "VE_Xplorer/SceneGraph/Utilities/ComputeBoundsVisitor.h"

#include <osg/Transform>
#include <osg/Drawable>
#include <osg/Geode>

using namespace VE_SceneGraph::Utilities;

ComputeBoundsVisitor::ComputeBoundsVisitor( TraversalMode traversalMode )
:
osg::NodeVisitor( traversalMode )
{
    ;
}

void ComputeBoundsVisitor::reset()
{
    _matrixStack.clear();
    _bb.init();
}

void ComputeBoundsVisitor::getPolytope( osg::Polytope& polytope, float margin ) const
{
    float delta = _bb.radius()*margin;
    polytope.add( osg::Plane( 0.0, 0.0, 1.0, -( _bb.zMin() - delta ) ) );
    polytope.add( osg::Plane( 0.0, 0.0, -1.0, ( _bb.zMax() + delta ) ) );

    polytope.add( osg::Plane( 1.0, 0.0, 0.0, -( _bb.xMin() - delta ) ) );
    polytope.add( osg::Plane( -1.0, 0.0, 0.0, ( _bb.xMax() + delta ) ) );

    polytope.add( osg::Plane( 0.0, 1.0, 0.0, -( _bb.yMin() - delta ) ) );
    polytope.add( osg::Plane( 0.0, -1.0, 0.0, ( _bb.yMax() + delta ) ) );
}

void ComputeBoundsVisitor::getBase( osg::Polytope& polytope, float margin ) const
{
    float delta = _bb.radius() * margin;
    polytope.add( osg::Plane( 0.0, 0.0, 1.0, -( _bb.zMin()-delta ) ) );
}

void ComputeBoundsVisitor::apply( osg::Node& node )
{
    traverse( node );
}

void ComputeBoundsVisitor::apply( osg::Transform& transform )
{
    osg::Matrix matrix;
    if( !_matrixStack.empty() )
    {
        matrix = _matrixStack.back();
    }

    transform.computeLocalToWorldMatrix( matrix, this );

    pushMatrix( matrix );

    traverse( transform );

    popMatrix();
}

void ComputeBoundsVisitor::apply( osg::Geode& geode )
{
    for( unsigned int i = 0; i < geode.getNumDrawables(); i++ )
    {
        applyDrawable( geode.getDrawable( i ) );
    }
}

void ComputeBoundsVisitor::applyDrawable( osg::Drawable* drawable )
{
    if( _matrixStack.empty() )
    {
        _bb.expandBy( drawable->getBound() );
    }
    else
    {
        osg::Matrix& matrix = _matrixStack.back();
        const osg::BoundingBox& dbb = drawable->getBound();
        if( dbb.valid() )
        {
            _bb.expandBy( dbb.corner( 0 ) * matrix );
            _bb.expandBy( dbb.corner( 1 ) * matrix );
            _bb.expandBy( dbb.corner( 2 ) * matrix );
            _bb.expandBy( dbb.corner( 3 ) * matrix );
            _bb.expandBy( dbb.corner( 4 ) * matrix );
            _bb.expandBy( dbb.corner( 5 ) * matrix );
            _bb.expandBy( dbb.corner( 6 ) * matrix );
            _bb.expandBy( dbb.corner( 7 ) * matrix );
        }
    }
}
