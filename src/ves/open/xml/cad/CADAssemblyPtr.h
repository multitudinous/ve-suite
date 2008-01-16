/*************** <auto-copyright.pl BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CAD_ASSEMBLY_PTR_H
#define CAD_ASSEMBLY_PTR_H


#include <ves/util/PointerTypes.h>

/**
 * \file
 *
 * Include this file to get a forward declaration of the pointer type
 * VE_XML::VE_CAD::CADAssembly.  To get the full
 * declaration of VE_XML::VE_CAD::CADAssembly
 * ves/open/xml/cad/CADAssembly.h must be included, too.
 */

namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class CADAssembly;
/// Typedef for a SmartPtr type for the Model.
typedef ves::util::ClassPtrDef<CADAssembly>::type  CADAssemblyPtr;
typedef ves::util::SharedPtrDef<CADAssembly>::type CADAssemblySharedPtr;
typedef ves::util::WeakPtrDef<CADAssembly>::type   CADAssemblyWeakPtr;
typedef ves::util::ScopedPtrDef<CADAssembly>::type CADAssemblyScopedPtr;

}
}
}
}
#endif//CAD_ASSEMBLY_PTR_H

