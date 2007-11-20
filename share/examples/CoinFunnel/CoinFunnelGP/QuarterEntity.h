#ifndef QUARTER_ENTITY_H
#define QUARTER_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- C/C++ Libraries --- //
#include <string>

namespace demo
{
class QuarterEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    QuarterEntity( std::string geomFile, ves::xplorer::scenegraph::DCS* pluginDCS );
    virtual ~QuarterEntity();

    void SetNameAndDescriptions( std::string geomFile );

private:

};
}

#endif // end SLIDE_ENTITY_H
