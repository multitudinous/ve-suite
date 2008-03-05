#ifndef FUNNEL_ENTITY_H
#define FUNNEL_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //

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

    void SetShaders();

private:
    void SetShaderOne();

    osg::ref_ptr< osg::Group > m_nonPhysicsGeometry;
    osg::ref_ptr< osg::Group > m_nonPhysicsGeometryII;
};
}

#endif // end FUNNEL_ENTITY_H
