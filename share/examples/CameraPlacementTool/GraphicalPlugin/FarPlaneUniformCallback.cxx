// --- My Includes --- //
#include "FarPlaneUniformCallback.h"

// --- OSG Includes --- //
#include <osg/NodeVisitor>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
FarPlaneUniformCallback::FarPlaneUniformCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
FarPlaneUniformCallback::FarPlaneUniformCallback(
    const FarPlaneUniformCallback& input )
:
osg::Object( input ),
osg::Uniform::Callback( input )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
FarPlaneUniformCallback::~FarPlaneUniformCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FarPlaneUniformCallback::operator()( osg::Uniform* uniform, osg::NodeVisitor* nv )
{
    if( uniform )
    {
        //uniform->set( static_cast< float >( time ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
