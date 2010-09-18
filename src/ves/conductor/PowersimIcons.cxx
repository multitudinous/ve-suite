/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

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
