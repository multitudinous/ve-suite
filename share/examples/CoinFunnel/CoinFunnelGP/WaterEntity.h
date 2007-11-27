#ifndef WATER_ENTITY_H
#define WATER_ENTITY_H

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
class UniformUpdateCallback;

class WaterEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    WaterEntity( std::string geomFile, ves::xplorer::scenegraph::DCS* pluginDCS );
    virtual ~WaterEntity();

    void SetNameAndDescriptions( std::string geomFile );

    void SetShaders( osg::TextureCubeMap* tcm );

private:
    void SetShaderOne( osg::TextureCubeMap* tcm );

};
}

#endif // end WATER_ENTITY_H
