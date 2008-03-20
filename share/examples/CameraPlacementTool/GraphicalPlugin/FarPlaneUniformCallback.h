#ifndef FAR_PLANE_UNIFORM_CALLBACK_H
#define FAR_PLANE_UNIFORM_CALLBACK_H

// --- OSG Includes --- //
#include <osg/Uniform>

namespace cpt
{
/*----------------------------------------------------------------------------*/
class FarPlaneUniformCallback : public osg::Uniform::Callback
{
public:
    FarPlaneUniformCallback();

    FarPlaneUniformCallback( const FarPlaneUniformCallback& input );

    virtual void operator()( osg::Uniform* uniform, osg::NodeVisitor* nv );

protected:
    virtual ~FarPlaneUniformCallback();

};
/*----------------------------------------------------------------------------*/
} //end cpt

#endif //FAR_PLANE_UNIFORM_CALLBACK_H
