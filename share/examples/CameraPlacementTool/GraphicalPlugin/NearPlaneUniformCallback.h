#ifndef NEAR_PLANE_UNIFORM_CALLBACK_H
#define NEAR_PLANE_UNIFORM_CALLBACK_H

// --- OSG Includes --- //
#include <osg/Uniform>

namespace cpt
{
/*----------------------------------------------------------------------------*/
class NearPlaneUniformCallback : public osg::Uniform::Callback
{
public:
    NearPlaneUniformCallback();

    NearPlaneUniformCallback( const NearPlaneUniformCallback& input );

    virtual void operator()( osg::Uniform* uniform, osg::NodeVisitor* nv );

protected:
    virtual ~NearPlaneUniformCallback();

};
/*----------------------------------------------------------------------------*/
} //end cpt

#endif //NEAR_PLANE_UNIFORM_CALLBACK_H
