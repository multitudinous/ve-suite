#ifndef SHADERS_H
#define SHADERS_H

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class StateSet;
}

class Shaders
{
public:
    Shaders();
    ~Shaders();

    osg::ref_ptr< osg::StateSet > Phong();
    osg::ref_ptr< osg::StateSet > XRay();
};

#endif //SHADERS_H