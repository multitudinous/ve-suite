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
 * Date modified: $Date: 2008-03-13 20:07:47 -0600 (Thu, 13 Mar 2008) $
 * Version:       $Rev: 10811 $
 * Author:        $Author: jaredabo $
 * Id:            $Id: TwoDStringArrayPtr.h 10811 2008-03-14 02:07:47Z jaredabo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef _VES_OPEN_XML_TWODSTRINGARRAY_PTR_H_
#define _VES_OPEN_XML_TWODSTRINGARRAY_PTR_H_

#include <ves/util/PointerTypes.h>

/**
 * \file ves/open/xml/TwoDStringArrayPtr.h
 *
 * Include this file to get a forward declaration of the type
 * ves::open::xml::TwoDStringArray and its pointer types.
 * For the full declaration of ves::open::xml::TwoDStringArray
 * ves/open/xml/TwoDStringArray.h must be included, too.
 */

namespace ves
{
namespace open
{
namespace xml
{
class TwoDStringArray;
/// Typedef for the SmartPtr types.
typedef ves::util::ClassPtrDef<TwoDStringArray>::type  TwoDStringArrayPtr;
typedef ves::util::SharedPtrDef<TwoDStringArray>::type TwoDStringArraySharedPtr;
typedef ves::util::WeakPtrDef<TwoDStringArray>::type   TwoDStringArrayWeakPtr;
typedef ves::util::ScopedPtrDef<TwoDStringArray>::type TwoDStringArrayScopedPtr;
}
}
}
#endif
