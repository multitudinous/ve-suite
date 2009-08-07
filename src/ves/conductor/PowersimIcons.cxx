
// --- VE-Suite Includes --- //
#include <ves/conductor/PowersimIcons.h>

#include <ves/conductor/xpm/powersim/Ps_COMPONENT.xpm>
#include <ves/conductor/xpm/powersim/Ps_PROJECT.xpm>
#include <ves/conductor/xpm/powersim/Ps_RANGES.xpm>
#include <ves/conductor/xpm/powersim/Ps_SIMULATION.xpm>
#include <ves/conductor/xpm/powersim/Ps_STUDIO.xpm>
#include <ves/conductor/xpm/powersim/Ps_UNITS.xpm>
#include <ves/conductor/xpm/powersim/Ps_VARIABLE_AUXILIARY.xpm>
#include <ves/conductor/xpm/powersim/Ps_VARIABLE_CONSTANT.xpm>
#include <ves/conductor/xpm/powersim/Ps_VARIABLE_LEVEL.xpm>
#include <ves/conductor/xpm/powersim/Ps_VARIABLE_MODEL.xpm>

////////////////////////////////////////////////////////////////////////////////
std::map< std::string, char** > GetPowersimIconMap()
{
    std::map< std::string, char** > tempIconMap;

    //tempIconMap[ "Ps_COMPONENT.xpm" ] = Ps_COMPONENT_xpm;
    //tempIconMap[ "Ps_PROJECT.xpm" ] = Ps_PROJECT_xpm;
    //tempIconMap[ "Ps_RANGES.xpm" ] = Ps_RANGES_xpm;
    //tempIconMap[ "Ps_SIMULATION.xpm" ] = Ps_SIMULATION_xpm;
    tempIconMap[ "Ps_STUDIO" ] = Ps_STUDIO_xpm;

    return tempIconMap;
}
////////////////////////////////////////////////////////////////////////////////
