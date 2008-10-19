// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix  Software LLC. All rights reserved.

#ifndef __DEBUG_BULLET_H__
#define __DEBUG_BULLET_H__

#include <osgBullet/Export.h>

#include <osg/Matrix>
#include <osg/ref_ptr>

namespace osg {
    class Node;
    class Group;
    class MatrixTransform;
}

namespace osgBullet
{

class OSGBULLET_EXPORT DebugBullet
{
public:
    DebugBullet();
    ~DebugBullet();

    unsigned int addDynamic( osg::MatrixTransform* mt );
    unsigned int addStatic( osg::Node* node );

    void setTransform( unsigned int idx, const osg::Matrix& m );

    osg::Node* getRoot() const;

    bool remove( osg::Node* node );

protected:
    osg::ref_ptr< osg::Group > _root;
};

}


#endif
