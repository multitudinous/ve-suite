// --- My Includes --- //
#include "ViewPositionUpdateCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Matrix.h>
#include <gmtl/Vec.h>

// --- OSG Includes --- //
#include <osg/NodeVisitor>
#include <osg/Vec3>

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
ViewPositionUpdateCallback::ViewPositionUpdateCallback()
{
    m_head.init( "VJHead" );
}
////////////////////////////////////////////////////////////////////////////////
ViewPositionUpdateCallback::ViewPositionUpdateCallback( const ViewPositionUpdateCallback& input )
:
osg::Object( input ),
osg::Uniform::Callback( input )
{
    m_head.init( "VJHead" );
}
////////////////////////////////////////////////////////////////////////////////
void ViewPositionUpdateCallback::operator()( osg::Uniform* uniform, osg::NodeVisitor* nv )
{
    if( uniform )
    {
        gmtl::Matrix44d headMatrix = convertTo< double >( m_head->getData() );
        gmtl::Point3d headPosition = gmtl::makeTrans< gmtl::Point3d >( headMatrix );
        osg::Vec3d viewPosition;
        viewPosition.set( headPosition.mData[ 0 ], headPosition.mData[ 1 ], headPosition.mData[ 2 ] );
        uniform->set( viewPosition );
    }
}
////////////////////////////////////////////////////////////////////////////////

} // end demo
