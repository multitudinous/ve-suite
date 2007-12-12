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

#include "osgOQ/QueryGeometry.h"
#include "osgOQ/OcclusionQueryContext.h"
#include <osg/CopyOp> // TBD implement copy
#include <osg/Referenced>
#include <osg/Geometry>
#include <osg/Notify>
#include <cassert>


namespace osgOQ
{


QueryGeometry::QueryGeometry( OcclusionQueryContext* oqc, const std::string& oqnName )
  : _oqc( oqc ),
    _oqnName( oqnName )
{
    // TBD check to see if we can have this on.
    setUseDisplayList( false );
}

#if POST_OSG_1_2
// After 1.2, param 1 changed from State to RenderInfo.
// Warning: Version was still 1.2 on dev branch long after the 1.2 release,
//   and finally got bumped to 1.9 in April 2007.
void
QueryGeometry::drawImplementation( osg::RenderInfo& renderInfo ) const
{
    unsigned int contextID = renderInfo.getState()->getContextID();
#else
void
QueryGeometry::drawImplementation( osg::State& state ) const
{
    unsigned int contextID = state.getContextID();
#endif
    TestResult& tr = _results[ contextID ];

    osg::Drawable::Extensions* ext = getExtensions( contextID, true );

    if (!tr._init)
    {
        ext->glGenQueries( 1, &(tr._id) );
        tr._init = true;
    }

    if (_oqc->getDebugVerbosity() > 0)
        osg::notify( osg::ALWAYS ) <<
        "QG: Querying for: " << _oqnName << std::endl;

    ext->glBeginQuery( GL_SAMPLES_PASSED_ARB, tr._id );
#if POST_OSG_1_2
    Geometry::drawImplementation( renderInfo );
#else
    Geometry::drawImplementation( state );
#endif
    ext->glEndQuery( GL_SAMPLES_PASSED_ARB );
    tr._active = true;

	if (_oqc->getStatistics())
		_oqc->incNumQueries();

	osg::notify( osg::DEBUG_INFO ) <<
		"osgOQ: QG. OQNName: " << _oqnName <<
		", Ctx: " << contextID <<
		", ID: " << tr._id << std::endl;
#ifdef _DEBUG
    {
        GLenum err;
        if ((err = glGetError()) != GL_NO_ERROR)
            osg::notify( osg::FATAL ) <<
            "osgOQ: QG: OpenGL error: " << err << "." << std::endl;
    }
#endif
}

unsigned int
QueryGeometry::retrieveQuery( unsigned int contextID )
{
    TestResult& tr = _results[ contextID ];

    if (!tr._active || !tr._init)
	    // This test wasn't executed last frame. This is probably because
	    //   a parent node failed the OQ test, this node is outside the
	    //   view volume, or we didn't run the test because we had not
        //   exceeded visibleQueryFrameCount.
	    // Do not obtain results from OpenGL.
	    return tr._numPixels;

    if (_oqc->getDebugVerbosity() > 0)
        osg::notify( osg::ALWAYS ) <<
        "QG: Retrieving for: " << _oqnName << std::endl;

    osg::Drawable::Extensions* ext = getExtensions( contextID, true );
#if POST_OSG_1_2
    ext->glGetQueryObjectiv( tr._id, GL_QUERY_RESULT, &(tr._numPixels) );
#else
    ext->glGetQueryObjectiv( tr._id, GL_QUERY_RESULT_ARB, &(tr._numPixels) );
#endif
    if (tr._numPixels < 0)
	    osg::notify( osg::WARN ) << "OcclusionQueryNode::traverse: " <<
		    "glGetQueryObjectiv returned negative value (" << tr._numPixels << ")." << std::endl;

    // Either retrieve last frame's results, or ignore it because the
    //   camera is inside the view. In either case, _active is now false.
    tr._active = false;

    return tr._numPixels;
}


}
