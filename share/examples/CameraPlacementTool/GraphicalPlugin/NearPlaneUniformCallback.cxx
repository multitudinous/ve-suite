// --- My Includes --- //
#include "NearPlaneUniformCallback.h"

// --- OSG Includes --- //
#include <osg/NodeVisitor>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
NearPlaneUniformCallback::NearPlaneUniformCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NearPlaneUniformCallback::NearPlaneUniformCallback(
    const NearPlaneUniformCallback& input )
:
osg::Object( input ),
osg::Uniform::Callback( input )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NearPlaneUniformCallback::~NearPlaneUniformCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void NearPlaneUniformCallback::operator()( osg::Uniform* uniform, osg::NodeVisitor* nv )
{
    if( uniform )
    {
        //uniform->set( static_cast< float >( time ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
