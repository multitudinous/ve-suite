#ifndef WATER_ENTITY_H
#define WATER_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- C/C++ Libraries --- //
#include <string>

namespace demo
{
class WaterEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    WaterEntity( std::string geomFile, ves::xplorer::scenegraph::DCS* pluginDCS );
    virtual ~WaterEntity();

    void SetNameAndDescriptions( std::string geomFile );

private:

};
}

#endif // end WATER_ENTITY_H
