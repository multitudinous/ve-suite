#ifndef TIME_UPDATE_CALLBACK_H
#define TIME_UPDATE_CALLBACK_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/Uniform>

namespace demo
{
class TimeUpdateCallback : public osg::Uniform::Callback
{
public:
    ///Constructor
    TimeUpdateCallback();

    ///Copy constructor
    TimeUpdateCallback( const TimeUpdateCallback& );

    virtual void operator()( osg::Uniform* uniform, osg::NodeVisitor* nv );

protected:
    ///Destructor
    virtual ~TimeUpdateCallback(){;}

};
} // end demo

#endif // end TIME_UPDATE_CALLBACK_H
