/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 * Date modified: $Date: 2009-05-06 14:32:42 -0600 (Wed, 06 May 2009) $
 * Version:       $Rev: 12657 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: ManipulatorPtr.h 12657 2009-05-06 20:32:42Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef MANIPULATOR_PTR_H
#define MANIPULATOR_PTR_H

// --- VE-Suite Includes --- //
#include <ves/util/PointerTypes.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
class Manipulator;

//Typedef for a SmartPtr type for the Manipulator
typedef ves::util::ClassPtrDef< Manipulator >::type ManipulatorPtr;
typedef ves::util::SharedPtrDef< Manipulator >::type ManipulatorSharedPtr;
typedef ves::util::WeakPtrDef< Manipulator >::type ManipulatorWeakPtr;
typedef ves::util::ScopedPtrDef< Manipulator >::type ManipulatorScopedPtr;

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //MANIPULATOR_PTR_H
