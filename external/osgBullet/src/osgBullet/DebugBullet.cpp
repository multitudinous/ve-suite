// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#include <osgBullet/DebugBullet.h>
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/PolygonMode>
#include <osg/PolygonOffset>


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
DebugBullet::addDynamic( osg::MatrixTransform* mt )
{
    _root->addChild( mt );
    return _root->getNumChildren() - 1;
}
unsigned int
DebugBullet::addStatic( osg::Node* node )
{
    osg::MatrixTransform* mt = new osg::MatrixTransform;
    mt->addChild( node );
    _root->addChild( mt );
    return _root->getNumChildren() - 1;
}

void
DebugBullet::setTransform( unsigned int idx, const osg::Matrix& m )
{
    osg::MatrixTransform* mt = dynamic_cast< osg::MatrixTransform* >( _root->getChild( idx ) );
    mt->setMatrix( m );
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
