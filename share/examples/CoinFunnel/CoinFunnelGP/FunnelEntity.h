#ifndef FUNNEL_ENTITY_H
#define FUNNEL_ENTITY_H

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
class FunnelEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    FunnelEntity( std::string geomFile,
                  ves::xplorer::scenegraph::DCS* pluginDCS,
                  ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~FunnelEntity();

    void SetNameAndDescriptions( std::string geomFile );

    void SetShaders( osg::TextureCubeMap* tcm );

private:
    void SetShaderOne( osg::TextureCubeMap* tcm );
    void SetShaderTwo();

    osg::ref_ptr< osg::Node > m_nonPhysicsGeometry;
};
}

#endif // end FUNNEL_ENTITY_H
