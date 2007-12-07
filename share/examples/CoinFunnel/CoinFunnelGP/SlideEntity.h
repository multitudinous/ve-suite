#ifndef SLIDE_ENTITY_H
#define SLIDE_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
namespace osg
{
    class Node;
}

// --- C/C++ Libraries --- //
#include <string>

namespace demo
{
class SlideEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    SlideEntity( std::string geomFile,
                 ves::xplorer::scenegraph::DCS* pluginDCS,
                 ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~SlideEntity();

    void SetNameAndDescriptions( std::string geomFile );

    void SetShaders();

private:
    void SetShaderOne();

    osg::ref_ptr< osg::Node > m_nonPhysicsGeometry;
};
}

#endif // end SLIDE_ENTITY_H
