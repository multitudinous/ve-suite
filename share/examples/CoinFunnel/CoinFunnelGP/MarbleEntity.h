#ifndef MARBLE_ENTITY_H
#define MARBLE_ENTITY_H

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

// --- OSG Includes --- //
namespace osg
{
    class TextureCubeMap;
}

// --- osgAL Includes --- //
#ifdef VE_SOUND
namespace osgAL
{
    class SoundManager;
}
#endif

// --- C/C++ Libraries --- //
#include <string>

namespace demo
{
class MarbleEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    MarbleEntity( std::string geomFile,
                  ves::xplorer::scenegraph::DCS* pluginDCS,
                  ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
                  ,osgAL::SoundManager* soundManager
#endif
                 );

    virtual ~MarbleEntity();

    void SetNameAndDescriptions( std::string geomFile );

    void SetShaders( osg::TextureCubeMap* tcm );

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* GetMarbleOnWoodSound();

    ves::xplorer::scenegraph::Sound* GetMarbleOnMetalSound();

    ves::xplorer::scenegraph::Sound* GetSound();
#endif

private:
    void SetShaderOne( osg::TextureCubeMap* tcm );
    void SetShaderTwo();

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* m_marbleOnWood;
    ves::xplorer::scenegraph::Sound* m_marbleOnMetal;
    ves::xplorer::scenegraph::Sound* m_marbleOnMarble;
#endif

    osg::ref_ptr< osg::Node > m_nonPhysicsGeometry;
};
}

#endif // end MARBLE_ENTITY_H
