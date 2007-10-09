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
#ifndef USER_PTR_H
#define USER_PTR_H

#include <VE_Open/XML/Util/PointerTypes.h>

/**
 * \file
 *
 * Include this file to get a forward declaration of the pointer type
 * VE_XML:UserPtr.  To get the full 
 * declaration of VE_XML::UserPtr
 * VE_Open/XML/User.h must be included, too.
 */

namespace VE_XML
{
   class User;
   /// Typedef for a SmartPtr type for the User.
   REGISTER_VE_PTR(User, UserPtr);
   REGISTER_VE_SHARED_PTR(User, UserSharedPtr);
   REGISTER_VE_WEAK_PTR(User, UserWeakPtr);
   REGISTER_VE_SCOPED_PTR(User, UserScopedPtr);
}

#endif
