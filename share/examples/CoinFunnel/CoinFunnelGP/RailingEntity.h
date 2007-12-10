#ifndef RAILING_ENTITY_H
#define RAILING_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
namespace osg
{
    class TextureCubeMap;
}

// --- C/C++ Libraries --- //
#include <string>

namespace demo
{
class RailingEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    RailingEntity( std::string geomFile,
                   ves::xplorer::scenegraph::DCS* pluginDCS,
                   ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~RailingEntity();

    void SetNameAndDescriptions( std::string geomFile );

    void SetShaders( osg::TextureCubeMap* tcm );

private:
    void SetShaderOne( osg::TextureCubeMap* tcm );

    osg::ref_ptr< osg::Node > m_nonPhysicsGeometry;
    osg::ref_ptr< osg::Node > m_nonPhysicsGeometryII;
};
}

#endif // end RAILING_ENTITY_H
