#ifndef VIEW_POSITION_UPDATE_CALLBACK_H
#define VIEW_POSITION_UPDATE_CALLBACK_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- vrJuggler Includes --- //
#include <gadget/Type/PositionInterface.h>

// --- OSG Includes --- //
#include <osg/Uniform>

namespace demo
{
class ViewPositionUpdateCallback : public osg::Uniform::Callback
{
public:
    ///Constructor
    ViewPositionUpdateCallback();

    ///Destructor
    virtual ~ViewPositionUpdateCallback(){;}

    ///Copy constructor
    ViewPositionUpdateCallback( const ViewPositionUpdateCallback& );

    virtual void operator()( osg::Uniform* uniform, osg::NodeVisitor* nv );

protected:
    gadget::PositionInterface m_head;
};
} // end demo

#endif // end VIEW_POSITION_UPDATE_CALLBACK_H
