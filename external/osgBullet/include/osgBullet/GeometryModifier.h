//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __GEOMETRY_MODIFIER_H__
#define __GEOMETRY_MODIFIER_H__

#include <osgBullet/Export.h>
#include <osgBullet/GeometryOperation.h>
#include <osg/NodeVisitor>
#include <osg/Geode>
#include <osg/Version>
#include <ostream>

namespace osgBullet {

class OSGBULLET_EXPORT GeometryModifier : public osg::NodeVisitor
{
public:
    GeometryModifier();
    GeometryModifier( GeometryOperation* geomOp );
    ~GeometryModifier();

#if( ( OPENSCENEGRAPH_MAJOR_VERSION >= 2) && (OPENSCENEGRAPH_MINOR_VERSION >= 8) )
    META_NodeVisitor(osgBullet,GeometryModifier)
#endif

    void setGeometryOperation( GeometryOperation* geomOp ) { _geomOp = geomOp; }
    GeometryOperation* getGeometryOperation() { return( _geomOp.get() ); }
    const GeometryOperation* getGeometryOperation() const { return( _geomOp.get() ); }

    void displayStatistics( std::ostream& ostr ) const;

    virtual void apply( osg::Geode& geode );

protected:
    osg::ref_ptr< GeometryOperation > _geomOp;

    unsigned int _drawableCount;
    unsigned int _geometryCount;
};

}

#endif
