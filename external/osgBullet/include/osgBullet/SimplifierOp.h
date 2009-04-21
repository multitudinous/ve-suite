//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __SIMPLIFIER_OP_H__
#define __SIMPLIFIER_OP_H__


#include <osgBullet/Export.h>
#include <osgBullet/GeometryOperation.h>
#include <osg/CopyOp>
#include <osg/Object>
#include <osgUtil/Simplifier>

namespace osgBullet {

class OSGBULLET_EXPORT SimplifierOp : public GeometryOperation
{
public:
    SimplifierOp();
    SimplifierOp( const SimplifierOp& rhs, const osg::CopyOp& copyOp=osg::CopyOp::SHALLOW_COPY );

    META_Object(osgBullet,SimplifierOp)

    virtual osg::Geometry* operator()( osg::Geometry& geom );

    // Make this public as a quick and dirty way to configure the simplification.
    osg::ref_ptr< osgUtil::Simplifier > _simplifier;

protected:
    ~SimplifierOp();

};

}

#endif