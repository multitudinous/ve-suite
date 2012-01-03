/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef VE_POINTER_TYPES_H
#define VE_POINTER_TYPES_H

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/scoped_ptr.hpp>

namespace ves
{
namespace util
{
// ClassPtrDef is the regular ptr class to use.
template
< typename T >
struct ClassPtrDef
{
    typedef boost::shared_ptr< T > type;
};

// SharedPtrDef is for using shared ptrs explicitly.
template
< typename T >
struct SharedPtrDef
{
    typedef boost::shared_ptr< T > type;
};

// WeakPtrDef used for getting around circular references only.
template
< typename T >
struct WeakPtrDef
{
    typedef boost::weak_ptr< T > type;
};

// Simple scoped ptr for use within functions only.  Very lightweight.
template
< typename T >
struct ScopedPtrDef
{
    typedef boost::scoped_ptr< T > type;
};

}
}
#endif

