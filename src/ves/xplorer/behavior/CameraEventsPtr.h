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
#pragma once

#include <ves/util/PointerTypes.h>

/**
 * \file
 *
 * Include this file to get a forward declaration of the type
 * ves::xplorer::behavior::CameraEvents and its pointer types.
 * For the full declaration of ves::xplorer::behavior::CameraEvents
 * ves/xplorer/behavior/CameraEvents.h must be included, too.
 */

namespace ves
{
namespace xplorer
{
namespace behavior
{
class CameraEvents;
/// Typedef for the SmartPtr types.
typedef ves::util::ClassPtrDef<CameraEvents>::type  CameraEventsPtr;
typedef ves::util::SharedPtrDef<CameraEvents>::type CameraEventsSharedPtr;
typedef ves::util::WeakPtrDef<CameraEvents>::type   CameraEventsWeakPtr;
typedef ves::util::ScopedPtrDef<CameraEvents>::type CameraEventsScopedPtr;
}
}
}
