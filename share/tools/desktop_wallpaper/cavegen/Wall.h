//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#ifndef __WALL_H__
#define __WALL_H__ 1


#include <osg/Node>
#include <osg/Geode>
#include <osg/Geometry>

#include <iostream>
#include <vector>
#include <string>



class Wall
{
public:
    Wall( std::istream& in );
    void dump( std::ostream& out );

    void createGeode( const osg::Matrix& m );
    osg::Geode* getGeode() const;
    osg::Matrix getProj() const;
    osg::Matrix getView() const;

    std::string suffix_;
    osg::Vec3 corner_;
    osg::Vec3 w_, h_;
    std::string texName_;

    osg::ref_ptr< osg::Geode > geode_;
};
typedef std::vector< Wall > WallList;

WallList readWallFile( const std::string& fName, osg::Vec3& viewPos, std::string& prefix );


#endif
