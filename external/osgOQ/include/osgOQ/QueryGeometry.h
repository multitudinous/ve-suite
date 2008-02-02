//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#ifndef OSGOQ_QUERY_GEOMETRY_H
#define OSGOQ_QUERY_GEOMETRY_H 1

#include <osg/Referenced>
#include <osg/Geometry>
#include <osg/Version>
#include <string>
#include <map>



namespace osgOQ
{


// TestResult -- stores (per context) results of an occlusion query
//   test performed by QueryGeometry. An OcclusionQueryNode has a
//   Geode owning a single QueryGeometry that
//   draws the occlusion query geometry. QueryGeometry keeps a
//   TestResult per context to store the result/status of each query.
// Accessed during the cull and draw traversals.
class TestResult : public osg::Referenced
{
public:
    TestResult() : _init( false ), _id( 0 ), _active( false ), _numPixels( 0 ) {}
    ~TestResult() {}

    bool _init;

    // Query ID for this context.
    GLuint _id;

    // Set to true when a query gets issued and set to
    //   false when the result is retrieved.
    mutable bool _active;

    // Result of last query.
    GLint _numPixels;
};

class QueryGeometry : public osg::Geometry
{
public:
    QueryGeometry( const std::string& oqnName=std::string("") );
    ~QueryGeometry() {}

    // TBD implement copy constructor

    virtual void drawImplementation( osg::RenderInfo& renderInfo ) const;

    unsigned int getNumPixels( osg::Camera* cam );

protected:
    typedef std::map< osg::Camera*, TestResult > ResultMap;
    mutable ResultMap _results;
    mutable OpenThreads::Mutex _mapMutex;

    // Needed for debug only
    std::string _oqnName;
};


}


#endif
