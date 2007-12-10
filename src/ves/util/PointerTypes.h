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


#include <loki/StrongPtr.h>
#include <loki/SmartPtr.h>

namespace ves
{
namespace util
{
// ClassPtrDef is the regular ptr class to use.
template
<
typename T,
bool Strong = true,
class OwnershipPolicy = Loki::LockableTwoRefCounts,
class ConversionPolicy = Loki::DisallowConversion,
template < class > class CheckingPolicy = Loki::AssertCheck,
template < class > class ResetPolicy = Loki::CantResetWithStrong,
template < class > class DeletePolicy = Loki::DeleteSingle
>
struct ClassPtrDef
{
    typedef Loki::StrongPtr
    <
    T,
    Strong,
    OwnershipPolicy,
    ConversionPolicy,
    CheckingPolicy,
    ResetPolicy,
    DeletePolicy
    >
    type;
};

// SharedPtrDef is for using shared ptrs explicitly.
template
<
typename T,
bool Strong = true,
class OwnershipPolicy = Loki::LockableTwoRefCounts,
class ConversionPolicy = Loki::DisallowConversion,
template < class > class CheckingPolicy = Loki::AssertCheck,
template < class > class ResetPolicy = Loki::CantResetWithStrong,
template < class > class DeletePolicy = Loki::DeleteSingle
>
struct SharedPtrDef
{
    typedef Loki::StrongPtr
    <
    T,
    Strong,
    OwnershipPolicy,
    ConversionPolicy,
    CheckingPolicy,
    ResetPolicy,
    DeletePolicy
    >
    type;
};

// WeakPtrDef used for getting around circular references only.
template
<
typename T,
bool Strong = false,
class OwnershipPolicy = Loki::LockableTwoRefCounts,
class ConversionPolicy = Loki::DisallowConversion,
template < class > class CheckingPolicy = Loki::AssertCheck,
template < class > class ResetPolicy = Loki::CantResetWithStrong,
template < class > class DeletePolicy = Loki::DeleteSingle
>
struct WeakPtrDef
{
    typedef Loki::StrongPtr
    <
    T,
    Strong,
    OwnershipPolicy,
    ConversionPolicy,
    CheckingPolicy,
    ResetPolicy,
    DeletePolicy
    >
    type;
};

// Simple scoped ptr for use within functions only.  Very lightweight.
template
<
typename T,
template <class> class OwnershipPolicy = Loki::NoCopy,
class ConversionPolicy = Loki::DisallowConversion,
template <class> class CheckingPolicy = Loki::AssertCheck,
template <class> class StoragePolicy = Loki::DefaultSPStorage,
template<class> class ConstnessPolicy = LOKI_DEFAULT_CONSTNESS
>
struct ScopedPtrDef
{
    typedef Loki::SmartPtr
    <
    T,
    OwnershipPolicy,
    ConversionPolicy,
    CheckingPolicy,
    StoragePolicy,
    ConstnessPolicy
    >
    type;
};

}
}
#endif

