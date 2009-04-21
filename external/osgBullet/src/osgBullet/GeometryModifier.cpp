//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgBullet/GeometryModifier.h>
#include <osgBullet/GeometryOperation.h>
#include <osg/Geode>
#include <osg/Geometry>
#include <ostream>

namespace osgBullet {

GeometryModifier::GeometryModifier()
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    _drawableCount( 0 ),
    _geometryCount( 0 )
{
}
GeometryModifier::GeometryModifier( GeometryOperation* geomOp )
  : osg::NodeVisitor( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN ),
    _geomOp( geomOp ),
    _drawableCount( 0 ),
    _geometryCount( 0 )
{
}

GeometryModifier::~GeometryModifier()
{
}

void
GeometryModifier::apply( osg::Geode& geode )
{
    for(unsigned int i=0;i<geode.getNumDrawables();++i)
    {
        _drawableCount++;
        osg::Geometry* geometry = geode.getDrawable(i)->asGeometry();
        if (geometry)
        {
            _geometryCount++;
            if( geometry->containsSharedArrays() )
                osg::notify( osg::ALWAYS ) << "Warning! Geometry contains shared arrays" << std::endl;
            osg::ref_ptr< osg::Geometry > newGeom = (*_geomOp)( *geometry );
            geode.replaceDrawable( geometry, newGeom.get() );
        }
    }
}

void
GeometryModifier::displayStatistics( std::ostream& ostr ) const
{
    ostr << "GeometryModifier statistics" << std::endl;
    ostr << "  GeometryOperation type: " << _geomOp->className() << std::endl;
    ostr << "  # Drawable: " << _drawableCount << ", # Geometry: " << _geometryCount << std::endl;
    ostr << "  Input vertices: " << _geomOp->getVerticesIn() << std::endl;
    ostr << "  Output vertices: " << _geomOp->getVerticesOut() << std::endl;
}

}
