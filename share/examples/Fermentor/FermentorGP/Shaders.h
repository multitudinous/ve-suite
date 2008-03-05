#ifndef SHADERS_H
#define SHADERS_H

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class Node;
    class StateSet;
}

class Shaders
{
public:
    Shaders();
    ~Shaders();

    void Phong( osg::ref_ptr< osg::Node > node );
    void XRay( osg::ref_ptr< osg::Node > node );

private:
    void Initialize();

    osg::ref_ptr< osg::StateSet > m_phong;
    osg::ref_ptr< osg::StateSet > m_xray;

};

#endif //SHADERS_H
