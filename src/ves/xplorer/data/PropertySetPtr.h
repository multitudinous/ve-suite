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
#ifndef VES_XPLORER_DATA_PROPERTYSET_PTR_H
#define VES_XPLORER_DATA_PROPERTYSET_PTR_H

#include <ves/util/PointerTypes.h>

/**
 * \file ves::xplorer::data::PropertySetPtr
 *
 * Include this file to get a forward declaration of the pointer.
 * To get the full declaration of this pointer include the non-Ptr header file.
 */
 
/*!\class ves::xplorer::data::PropertySetPtr
 *
 */

/*!\namespace ves::xplorer::data
 *
 */

namespace ves
{
namespace xplorer
{
namespace data
{
class PropertySet;
/// Typedef for a SmartPtr
typedef ves::util::ClassPtrDef<PropertySet>::type  PropertySetPtr;
typedef ves::util::SharedPtrDef<PropertySet>::type PropertySetSharedPtr;
typedef ves::util::WeakPtrDef<PropertySet>::type   PropertySetWeakPtr;
typedef ves::util::ScopedPtrDef<PropertySet>::type PropertySetScopedPtr;
}
}
}
#endif
