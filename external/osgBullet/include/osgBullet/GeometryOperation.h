//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __GEOMETRY_OPERATION_H__
#define __GEOMETRY_OPERATION_H__

#include <osgBullet/Export.h>
#include <osg/Object>
#include <osg/Geometry>

namespace osgBullet {

class OSGBULLET_EXPORT GeometryOperation : public osg::Object
{
public:
    GeometryOperation();

    // Derived classes must override this method and return
    // a pointer to a new Geometry object.
    virtual osg::Geometry* operator()( osg::Geometry& geom ) = 0;

    // Statistics
    // Number of input vertices.
    void setVerticesIn( int n ) { _vertsIn = n; }
    void incVerticesIn( int n ) { _vertsIn += n; }
    int getVerticesIn() const { return( _vertsIn ); }

    // Number of outp0ut vertices.
    void setVerticesOut( int n ) { _vertsOut = n; }
    void incVerticesOut( int n ) { _vertsOut += n; }
    int getVerticesOut() const { return( _vertsOut ); }

protected:
    virtual ~GeometryOperation();

    int _vertsIn, _vertsOut;
};

}

#endif
