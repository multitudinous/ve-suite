#ifndef WATER_ENTITY_H
#define WATER_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

#ifdef VE_SOUND
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class Sound;
}
}
}
#endif

// --- osgAL Includes --- //
#ifdef VE_SOUND
namespace osgAL
{
    class SoundManager;
}
#endif

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
    WaterEntity( std::string geomFile,
                 ves::xplorer::scenegraph::DCS* pluginDCS
#ifdef VE_SOUND
                  ,osgAL::SoundManager* soundManager
#endif
                );

    virtual ~WaterEntity();

    void SetNameAndDescriptions( std::string geomFile );

    void SetShaders( osg::TextureCubeMap* tcm );

private:
    void SetShaderOne( osg::TextureCubeMap* tcm );

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* m_water;
#endif

};
}

#endif // end WATER_ENTITY_H
