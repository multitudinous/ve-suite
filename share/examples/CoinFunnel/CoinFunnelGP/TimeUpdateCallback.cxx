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
    if( uniform )
    {
        const osg::FrameStamp* frameStamp = nv->getFrameStamp();
        double time = frameStamp->getSimulationTime();

        uniform->set( static_cast< float >( time ) );
    }
}
////////////////////////////////////////////////////////////////////////////////

} // end demo
