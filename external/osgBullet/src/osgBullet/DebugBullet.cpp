// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#include <osgBullet/DebugBullet.h>
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/PolygonMode>
#include <osg/PolygonOffset>
#include <osg/Notify>

#include <osgBullet/AbsoluteModelTransform.h>


namespace osgBullet
{

DebugBullet::DebugBullet()
{
    _root = new osg::Group;

    osg::StateSet* state = _root->getOrCreateStateSet();
    osg::PolygonMode* pm = new osg::PolygonMode( osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
    state->setAttributeAndModes( pm );
    osg::PolygonOffset* po = new osg::PolygonOffset( -1, -1 );
    state->setAttributeAndModes( po );
    state->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
}
DebugBullet::~DebugBullet()
{
}

unsigned int
DebugBullet::addDynamic( osg::Transform* tr )
{
    _root->addChild( tr );
    return _root->getNumChildren() - 1;
}
unsigned int
DebugBullet::addStatic( osg::Node* node )
{
    _root->addChild( node );
    return _root->getNumChildren() - 1;
}

void
DebugBullet::setTransform( unsigned int idx, const osg::Matrix& m )
{
    osg::Node* node( _root->getChild( idx ) );

    osg::MatrixTransform* mt( NULL );
    osgBullet::AbsoluteModelTransform* amt( NULL );
    if( mt = dynamic_cast< osg::MatrixTransform* >( node ) )
        mt->setMatrix( m );
    else if( amt = dynamic_cast< osgBullet::AbsoluteModelTransform* >( node ) )
        amt->setMatrix( m );
    else
        osg::notify( osg::WARN ) << "DebugBullet: Unable to setTransform for index " << idx <<
        " with class name " << node->className() << std::endl;
}

osg::Node*
DebugBullet::getRoot() const
{
    return _root.get();
}

bool
DebugBullet::remove( osg::Node* node )
{
    return( _root->removeChild( node ) );
}


}
