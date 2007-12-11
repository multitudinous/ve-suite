#ifndef QUARTER_ENTITY_H
#define QUARTER_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
namespace osg
{
    class Texture2D;
}

// --- C/C++ Libraries --- //
#include <string>

namespace demo
{
class QuarterEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    QuarterEntity( std::string geomFile,
                   ves::xplorer::scenegraph::DCS* pluginDCS,
                   ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~QuarterEntity();

    void SetNameAndDescriptions( std::string geomFile );

    void SetShaders();

private:
    void SetShaderOne( osg::Node* node, osg::Texture2D* texture );

    osg::ref_ptr< osg::Node > m_nonPhysicsGeometry;
    osg::ref_ptr< osg::Node > m_nonPhysicsGeometryII;

};
}

#endif // end SLIDE_ENTITY_H
