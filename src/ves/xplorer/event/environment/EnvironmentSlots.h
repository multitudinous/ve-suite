/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef VES_XPLORER_EVENT_ENVIRONMENT_SLOTS_H
#define VES_XPLORER_EVENT_ENVIRONMENT_SLOTS_H

#include <ves/VEConfig.h>

#include <string>
#include <vector>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace environment
{
/*!\file EnvironmentSlots.h ves/xplorer/event/environment/EnvironmentSlots.h
 *   Class for changing environmental signals
 * \namespace ves::xplorer::event::environment
 */
///Enable the physics debugger
void EnablePhysicsDebugging( bool const& enable );
///Display the on-screen frame rate
void DisplayFrameRate( bool const& display );
///Display the on-screen coordinate system
void DisplayCoordinateSystem( bool const& display );
///Set the filename for an ambient audio filename
void SetAmbientAudioFile( std::string const& filename );
///Set the clear colors
void UpdateBackgroundColor( bool const enable, std::vector< double > const& color );
} //end environment
} //end event
} //end xplorer
} //end ves

#endif //VES_XPLORER_EVENT_ENVIRONMENT_SLOTS_H
