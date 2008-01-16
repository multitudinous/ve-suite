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
* Date modified: $Date: 2007-12-10 17:37:51 -0600 (Mon, 10 Dec 2007) $
* Version:       $Rev: 10148 $
* Author:        $Author: dshipton $
* Id:            $Id: TransformPtr.h 10148 2007-12-10 23:37:51Z dshipton $
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_TRANSFORM_PTR_H
#define VE_TRANFORM_PTR_H

#include <ves/util/PointerTypes.h>

/**
 * \file
 *
 * Include this file to get a forward declaration of the pointer type
 * VE_XML::TransformPtr.  To get the full
 * declaration of VE_XML::TransformPtr
 * VE_Open/XML/Transform.h must be included, too.
 */

namespace ves
{
namespace open
{
namespace xml
{
class Transform;
/// Typedef for a SmartPtr type for the Transform.
typedef ves::util::ClassPtrDef<Transform>::type  TransformPtr;
typedef ves::util::SharedPtrDef<Transform>::type TransformSharedPtr;
typedef ves::util::WeakPtrDef<Transform>::type   TransformWeakPtr;
typedef ves::util::ScopedPtrDef<Transform>::type TransformScopedPtr;
}
}
}
#endif
