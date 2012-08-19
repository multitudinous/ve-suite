/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#ifndef VES_UTIL_MOUSE_SIGNAL_SIGNATURE_H
#define VES_UTIL_MOUSE_SIGNAL_SIGNATURE_H

#include <switchwire/Event.h>

#include <switchwire/BooleanPropagationCombiner.h>

#include <gadget/Type/KeyboardMouse/Keys.h>

namespace osg
{
class Vec3d;
}

namespace ves
{
namespace util
{
/// signal for generating the start and end point for selection and other
///interaction tools.
/// Params are: start point and end point
typedef switchwire::Event< void ( osg::Vec3d const&, osg::Vec3d const& ) > StartEndPointSignal_type;

/// MouseMove signal
/// Params are: x, y, z, state (modifier mask OR'd with button mask)
typedef boost::signals2::signal < bool ( int, int, int, int ),
        switchwire::BooleanPropagationCombiner > MouseMoveSignal_type;

/// MouseDoubleClick signal
/// Params are: button, x, y, z, state (modifier mask OR'd with button mask)
typedef boost::signals2::signal < bool ( gadget::Keys, int, int, int, int ),
        switchwire::BooleanPropagationCombiner > MouseDoubleClickSignal_type;

/// Scroll signal type
/// Params are: deltaX, deltaY, x, y, state (modifier mask OR'd with button mask)
typedef boost::signals2::signal < bool ( int, int, int, int, int ),
        switchwire::BooleanPropagationCombiner > ScrollSignal_type;

/// ButtonPress signal type
/// Params are: button, x, y, state (modifier mask OR'd with button mask)
typedef boost::signals2::signal < bool ( gadget::Keys, int, int, int ),
        switchwire::BooleanPropagationCombiner > ButtonPressSignal_type;

/// ButtonRelease signal type
typedef boost::signals2::signal < bool ( gadget::Keys, int, int, int ),
        switchwire::BooleanPropagationCombiner > ButtonReleaseSignal_type;

/// KeyPress signal type
/// First arg is the key that was pressed
/// Second arg is the gadget::ModiferMask (modifier mask)
/// Third arg is the unicode representation of the key
///NOTE: As soon as VR Juggler supports wide body chars we can change the
///char argument back to a wchar_t
typedef boost::signals2::signal < bool ( gadget::Keys, int, char ),
        switchwire::BooleanPropagationCombiner > KeyPressSignal_type;

typedef switchwire::Event< bool ( gadget::Keys, int, char ) > KeyPressSignal_rtype;

/// KeyRelease signal type
/// First arg is the key that was pressed
/// Second arg is the gadget::ModiferMask (modifier mask)
/// Third arg is the unicode representation of the key
///NOTE: As soon as VR Juggler supports wide body chars we can change the
///char argument back to a wchar_t
typedef boost::signals2::signal < bool ( gadget::Keys, int, char ),
        switchwire::BooleanPropagationCombiner > KeyReleaseSignal_type;

typedef switchwire::Event< bool ( gadget::Keys, int, char ) > KeyReleaseSignal_rtype;
}
}
#endif
