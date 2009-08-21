// --- Header Includes --- //
#include "PolyVisualizationBase.h"

// --- C++ Includes --- ///
#include <iostream>

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/Geode.h>
#include <ves/xplorer/scenegraph/DCS.h>

using namespace vascularsimviewer;

//Constructor
PolyVisualizationBase::PolyVisualizationBase()
    :
    mPluginDCS( NULL ),
    mGeode( NULL )
{
    ;
}
//Destructor
PolyVisualizationBase::~PolyVisualizationBase()
{
    ;
}
