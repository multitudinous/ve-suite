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


// After the 1.2 release and leading up to the 2.0 release, OSG came out in a
//   series of "engineering releases" starting with 1.9.0, which changed the
//   OSG interface in an incompatible way from OSG v1.2.
#define POST_OSG_1_2 \
    ( (OSG_VERSION_MAJOR>1) || \
      ( (OSG_VERSION_MAJOR==1) && (OSG_VERSION_MINOR>2) ) )
#define PRE_OSG_1_9 \
    ( (OSG_VERSION_MAJOR<1) || \
      ( (OSG_VERSION_MAJOR==1) && (OSG_VERSION_MINOR<9) ) )


namespace osgOQ
{


class OcclusionQueryContext;


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
    QueryGeometry( OcclusionQueryContext* oqc, const std::string& oqnName=std::string("") );
    ~QueryGeometry() {}

    // TBD implement copy constructor

#if POST_OSG_1_2
	// After 1.2, param 1 changed from State to RenderInfo.
	// Warning: Version was still 1.2 on dev branch long after the 1.2 release,
	//   and finally got bumped to 1.9 in April 2007.
    virtual void drawImplementation( osg::RenderInfo& renderInfo ) const;
#else
    virtual void drawImplementation( osg::State& state ) const;
#endif

    unsigned int retrieveQuery( unsigned int contextID );

protected:
    typedef osg::buffered_object< TestResult > ResultList;
    mutable ResultList _results;

    // Needed for debug only
    std::string _oqnName;

    osg::ref_ptr<OcclusionQueryContext> _oqc;
};


}


#endif
