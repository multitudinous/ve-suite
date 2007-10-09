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
* Date modified: $Date: 2007-08-24 11:53:30 -0500 (Fri, 24 Aug 2007) $
* Version:       $Rev: 8827 $
* Author:        $Author: mccdo $
* Id:            $Id: CommandStrongPtr.h 8827 2007-08-24 16:53:30Z mccdo $
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_POINTER_TYPES_H
#define VE_POINTER_TYPES_H

/**
* \file
 *
 * Include this file to get a forward declaration of the pointer type
 * VE_XML::CommandStrongPtr.  To get the full 
 * declaration of VE_XML::CommandStrongPtr
 * VE_Open/XML/Command.h must be included, too.
 */

#define _USE_LOKI_POINTERS 1

#ifdef _USE_LOKI_POINTERS
#include <loki/StrongPtr.h>
#include <loki/SmartPtr.h>
#else
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/intrusive_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#endif

namespace VE_XML
{
#ifdef _USE_LOKI_POINTERS

   #define REGISTER_VE_PTR(classtype, name)                                     \
    typedef Loki::StrongPtr< classtype, true, Loki::LockableTwoRefCounts,       \
        Loki::DisallowConversion, Loki::AssertCheck, Loki::CantResetWithStrong, \
        Loki::DeleteSingle > name;                                              \

   #define REGISTER_VE_SHARED_PTR(classtype, name)                              \
    typedef Loki::StrongPtr< classtype, true, Loki::LockableTwoRefCounts,       \
        Loki::DisallowConversion, Loki::AssertCheck, Loki::CantResetWithStrong, \
        Loki::DeleteSingle > name;                                              \

   #define REGISTER_VE_WEAK_PTR(classtype, name)                                \
    typedef Loki::StrongPtr< classtype, false, Loki::LockableTwoRefCounts,      \
        Loki::DisallowConversion, Loki::AssertCheck, Loki::CantResetWithStrong, \
        Loki::DeleteSingle > name;                                              \

   #define REGISTER_VE_SCOPED_PTR(classtype, name)                              \
    typedef Loki::SmartPtr<classtype, Loki::NoCopy, Loki::DisallowConversion,   \
        Loki::DefaultSPStorage, LOKI_DEFAULT_CONSTNESS> name;                   \

#else

   #define REGISTER_VE_PTR(classtype, name)           \
    typedef boost::shared_ptr< classtype > name;      \

   #define REGISTER_VE_SHARED_PTR(classtype, name)    \
    typedef boost::shared_ptr< classtype > name;      \

   #define REGISTER_VE_WEAK_PTR(classtype, name)      \
    typedef boost::weak_ptr< classtype > name;        \

   #define REGISTER_VE_SCOPED_PTR(classtype, name)    \
    typedef boost::scoped_ptr< classtype > name;      \


#endif

}
#endif

