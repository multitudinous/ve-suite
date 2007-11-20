#ifndef RAMP_ENTITY_H
#define RAMP_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- C/C++ Libraries --- //
#include <string>

namespace demo
{
class RampEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    RampEntity( std::string geomFile, ves::xplorer::scenegraph::DCS* pluginDCS );
    virtual ~RampEntity();

    void SetNameAndDescriptions( std::string geomFile );

private:

};
}

#endif // end RAMP_ENTITY_H
