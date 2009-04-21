//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgBullet/SimplifierOp.h>
#include <osg/CopyOp>
#include <osgUtil/Simplifier>

namespace osgBullet {

SimplifierOp::SimplifierOp()
{
    _simplifier = new osgUtil::Simplifier( .2 );
}
SimplifierOp::SimplifierOp( const SimplifierOp& rhs, const osg::CopyOp& copyOp )
{
    _simplifier = rhs._simplifier;
}
SimplifierOp::~SimplifierOp()
{
}

osg::Geometry*
SimplifierOp::operator()( osg::Geometry& geom )
{
    _simplifier->simplify( geom );
    return( &geom );
}

}
