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
#include <osg/CopyOp> // TBD implement copy
#include <osg/Referenced>
#include <osg/Geometry>
#include <osg/Notify>
#include <osg/Timer>
#include <cassert>


namespace osgOQ
{


struct RetrieveQueriesCallback : public osg::Camera::DrawCallback
{
    typedef std::vector<TestResult*> ResultsVector;
    ResultsVector _results;

    RetrieveQueriesCallback( osg::Drawable::Extensions* ext=NULL )
      : _extensionsFallback( ext )
    {
    }

    RetrieveQueriesCallback( const RetrieveQueriesCallback&, const osg::CopyOp& ) {}
    META_Object( osgOQ, RetrieveQueriesCallback )

    virtual void operator() (const osg::Camera& camera) const
    {
        if (_results.empty())
            return;

        const osg::Timer& timer = *osg::Timer::instance();
        osg::Timer_t start_tick = timer.tick();
        double elapsedTime( 0. );
        int count( 0 );

        osg::Drawable::Extensions* ext;
        if (camera.getGraphicsContext())
        {
            // The typical path, for osgViewer-based applications or any
            //   app that has set up a valid GraphicsCOntext for the Camera.
            unsigned int contextID = camera.getGraphicsContext()->getState()->getContextID();
            RetrieveQueriesCallback* const_this = const_cast<RetrieveQueriesCallback*>( this );
            ext = const_this->getExtensions( contextID, true );
        }
        else
        {
            // No valid GraphicsContext in the Camera. This might happen in
            //   SceneView-based apps. Rely on the creating code to have passed
            //   in a valid Extensions pointer, and hope it's valid for any
            //   context that might be current.
            osg::notify( osg::DEBUG_INFO ) << "osgOQ: RQCB: Using fallback path to obtain Extensions pointer." << std::endl;
            ext = _extensionsFallback;
            if (!ext)
            {
                osg::notify( osg::FATAL ) << "osgOQ: RQCB: Extensions pointer fallback is NULL." << std::endl;
                return;
            }
        }

        ResultsVector::const_iterator it = _results.begin();
        while (it != _results.end())
        {
            TestResult* tr = const_cast<TestResult*>( *it );

            if (!tr->_active || !tr->_init)
            {
                // This test wasn't executed last frame. This is probably because
                //   a parent node failed the OQ test, this node is outside the
                //   view volume, or we didn't run the test because we had not
                //   exceeded visibleQueryFrameCount.
                // Do not obtain results from OpenGL.
                it++;
                continue;
            }

            osg::notify( osg::DEBUG_INFO ) <<
                "osgOQ: RQCB: Retrieving..." << std::endl;

            ext->glGetQueryObjectiv( tr->_id, GL_QUERY_RESULT, &(tr->_numPixels) );
            if (tr->_numPixels < 0)
                osg::notify( osg::WARN ) << "osgOQ: RQCB: " <<
                "glGetQueryObjectiv returned negative value (" << tr->_numPixels << ")." << std::endl;

            // Either retrieve last frame's results, or ignore it because the
            //   camera is inside the view. In either case, _active is now false.
            tr->_active = false;

            it++;
            count++;
        }

        elapsedTime = timer.delta_s(start_tick,timer.tick());
        osg::notify( osg::INFO ) << "osgOQ: RQCB: " << "Retrieved " << count <<
            " queries in " << elapsedTime << " seconds." << std::endl;
    }

    void reset()
    {
        _results.clear();
    }

    void add( TestResult* tr )
    {
        _results.push_back( tr );
    }

    osg::Drawable::Extensions* getExtensions( unsigned int contextID, bool createIfNotInitalized )
    {
        if (!s_extensions[ contextID ] && createIfNotInitalized)
            s_extensions[ contextID ] = new osg::Drawable::Extensions( contextID );
        return s_extensions[ contextID ].get();
    }

    typedef osg::buffered_value< osg::ref_ptr< osg::Drawable::Extensions > > BufferedExtensions;
    static BufferedExtensions s_extensions;

    osg::Drawable::Extensions* _extensionsFallback;
};

RetrieveQueriesCallback::BufferedExtensions RetrieveQueriesCallback::s_extensions;



// PreDraw callback; clears the list of Results from the PostDrawCallback (above).
struct ClearQueriesCallback : public osg::Camera::DrawCallback
{
    ClearQueriesCallback() : _rqcb( NULL ) {}
    ClearQueriesCallback( const ClearQueriesCallback&, const osg::CopyOp& ) {}
    META_Object( osgOQ, ClearQueriesCallback )

    virtual void operator() (const osg::Camera& camera) const
    {
        if (!_rqcb)
        {
            osg::notify( osg::FATAL ) << "oagOQ: CQCB: Invalid RQCB." << std::endl;
            return;
        }
        _rqcb->reset();
    }

    RetrieveQueriesCallback* _rqcb;
};


QueryGeometry::QueryGeometry( const std::string& oqnName )
  : _oqnName( oqnName )
{
    // TBD check to see if we can have this on.
    setUseDisplayList( false );
}

// After 1.2, param 1 changed from State to RenderInfo.
// Warning: Version was still 1.2 on dev branch long after the 1.2 release,
//   and finally got bumped to 1.9 in April 2007.
void
QueryGeometry::drawImplementation( osg::RenderInfo& renderInfo ) const
{
    unsigned int contextID = renderInfo.getState()->getContextID();
    osg::Drawable::Extensions* ext = getExtensions( contextID, true );
    osg::Camera* cam = renderInfo.getCurrentCamera();

    // Add callbacks if necessary.
    if (!cam->getPostDrawCallback())
    {
        RetrieveQueriesCallback* rqcb = new RetrieveQueriesCallback( ext );
        cam->setPostDrawCallback( rqcb );

        ClearQueriesCallback* cqcb = new ClearQueriesCallback;
        cqcb->_rqcb = rqcb;
        cam->setPreDrawCallback( cqcb );
    }

    // Get TestResult from Camera map
    TestResult* tr;
    {
        OpenThreads::ScopedLock<OpenThreads::Mutex> lock( _mapMutex );
        tr = &( _results[ cam ] );
    }

    // Add TestResult to RQCB.
    RetrieveQueriesCallback* rqcb = dynamic_cast<
        RetrieveQueriesCallback* >( cam->getPostDrawCallback() );
    if (!rqcb)
    {
        osg::notify( osg::FATAL ) << "oagOQ: QG: Invalid RQCB." << std::endl;
        return;
    }
    rqcb->add( tr );


    // Issue query
    if (!tr->_init)
    {
        ext->glGenQueries( 1, &(tr->_id) );
        tr->_init = true;
    }

    osg::notify( osg::DEBUG_INFO ) <<
        "oagOQ: QG: Querying for: " << _oqnName << std::endl;

    ext->glBeginQuery( GL_SAMPLES_PASSED_ARB, tr->_id );
    Geometry::drawImplementation( renderInfo );
    ext->glEndQuery( GL_SAMPLES_PASSED_ARB );
    tr->_active = true;


    osg::notify( osg::DEBUG_INFO ) <<
        "osgOQ: QG. OQNName: " << _oqnName <<
        ", Ctx: " << contextID <<
        ", ID: " << tr->_id << std::endl;
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
QueryGeometry::getNumPixels( osg::Camera* cam )
{
    TestResult tr;
    {
        OpenThreads::ScopedLock<OpenThreads::Mutex> lock( _mapMutex );
        tr =  _results[ cam ];
    }
    return tr._numPixels;
}


}
