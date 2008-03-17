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
class MarbleEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    MarbleEntity( std::string geomFile,
                  ves::xplorer::scenegraph::DCS* pluginDCS,
                  ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
                , osgAL::SoundManager* soundManager
#endif
                 );

    virtual ~MarbleEntity();

    void SetNameAndDescriptions( const std::string& geomFile );

    void SetShaders( osg::TextureCubeMap* tcm );

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* GetMarbleOnWoodSound();
    ves::xplorer::scenegraph::Sound* GetMarbleOnMetalSound();
    ves::xplorer::scenegraph::Sound* GetMarbleOnMarbleSound();
#endif

private:
//#if 0
   //terrible names
    void SetShaderOne( osg::TextureCubeMap* tcm );
    void SetShaderTwo();
//#endif

#ifdef VE_SOUND
    ves::xplorer::scenegraph::Sound* mMarbleOnWood;
    ves::xplorer::scenegraph::Sound* mMarbleOnMetal;
    ves::xplorer::scenegraph::Sound* mMarbleOnMarble;
#endif

    osg::ref_ptr< osg::Node > mNonPhysicsGeometry;
};
}

#endif // end MARBLE_ENTITY_H
