// --- My Includes --- //
#include "TimeUpdateCallback.h"

// --- OSG Includes --- //
#include <osg/NodeVisitor>
#include <osg/FrameStamp>

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
TimeUpdateCallback::TimeUpdateCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TimeUpdateCallback::TimeUpdateCallback( const TimeUpdateCallback& input )
:
osg::Object( input ),
osg::Uniform::Callback( input )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TimeUpdateCallback::operator()( osg::Uniform* uniform, osg::NodeVisitor* nv )
{
    const osg::FrameStamp* frameStamp = nv->getFrameStamp();
    double time = frameStamp->getSimulationTime();

    if( uniform )
    {
        uniform->set( static_cast< float >( time ) );
    }
}
////////////////////////////////////////////////////////////////////////////////

} // end demo
