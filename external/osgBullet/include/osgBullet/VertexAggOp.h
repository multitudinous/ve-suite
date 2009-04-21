//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __VERTEX_AGG_OP_H__
#define __VERTEX_AGG_OP_H__


#include <osgBullet/Export.h>
#include <osgBullet/GeometryOperation.h>
#include <osg/CopyOp>
#include <osg/Object>
#include <osg/Vec3>

namespace osgBullet {

struct Octree;

class OSGBULLET_EXPORT VertexAggOp : public GeometryOperation
{
public:
    VertexAggOp();
    VertexAggOp( const VertexAggOp& rhs, const osg::CopyOp& copyOp=osg::CopyOp::SHALLOW_COPY );

    META_Object(osgBullet,VertexAggOp)

    virtual osg::Geometry* operator()( osg::Geometry& geom );

    void setMaxVertsPerCell( unsigned int n ) { _maxVertsPerCell = n; }
    unsigned int getMaxVertsPerCell() const { return( _maxVertsPerCell ); }

    void setMinCellSize( osg::Vec3 v ) { _minCellSize = v; _useMinCellSize = true; }
    const osg::Vec3& getMinCellSize() const { return( _minCellSize ); }
    void setUseMinCellSize( bool use ) { _useMinCellSize = use; }
    bool getUseMinCellSize() const { return( _useMinCellSize ); }

    void setCreateHullPerGeometry( bool createHull ) { _createHull = createHull; }
    bool setCreateHullPerGeometry() const { return( _createHull ); }

    typedef enum {
        GEOMETRIC_MEAN,
        BOUNDING_BOX_CENTER,
    } PointSelectionMethod;

    void setPointSelectionMethod( PointSelectionMethod psm ) { _psm = psm; }
    PointSelectionMethod getPointSelectionMethod() const { return( _psm ); }

protected:
    ~VertexAggOp();

    void recurseBuild( Octree* cell ) const;
    void gatherVerts( Octree* cell, osg::Vec3Array* verts ) const;
    osg::Vec3 representativeVert( osg::Vec3Array* verts ) const;
    void createHull( osg::Geometry& geom );

    unsigned int _maxVertsPerCell;
    osg::Vec3 _minCellSize;
    bool _useMinCellSize;
    bool _createHull;
    PointSelectionMethod _psm;
};

}

#endif
