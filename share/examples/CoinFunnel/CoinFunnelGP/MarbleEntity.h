#ifndef MARBLE_ENTITY_H
#define MARBLE_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

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
    MarbleEntity( std::string geomFile, ves::xplorer::scenegraph::DCS* pluginDCS );
    virtual ~MarbleEntity();

    void SetNameAndDescriptions( std::string geomFile );

    void SetShaders( osg::TextureCubeMap* tcm );

    void SetSounds();

    ves::xplorer::scenegraph::Sound* GetSound();

private:
    void SetShaderOne( osg::TextureCubeMap* tcm );
    void SetShaderTwo();

    ves::xplorer::scenegraph::Sound* m_sound;
    osg::ref_ptr< osg::Node > m_nonPhysicsGeometry;
};
}

#endif // end MARBLE_ENTITY_H
