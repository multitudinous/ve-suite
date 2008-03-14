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
#if 0
 pass by const reference
    void SetNameAndDescriptions( std::string geomFile );
#endif

    void SetShaders();

private:
    void SetShaderOne();

#if 0
    osg::ref_ptr< osg::Group > mNonPhysicsGeometry;
                            this is a terrible variable name
    osg::ref_ptr< osg::Group > mNonPhysicsGeometryII;
#endif
};
}

#endif // end FUNNEL_ENTITY_H
