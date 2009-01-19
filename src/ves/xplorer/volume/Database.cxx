/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> **************/
#include <ves/xplorer/volume/Database.h>

/// The following is from the Loki::Singleton library.  This allows a windows
/// DLL to have a Singleton with exactly one instance.  This example is
/// take from the Singleton test case in the Loki source tree, and the macro
/// MUST be called from a source file (as opposed to a header file) to work
/// correctly.
typedef Loki::SingletonHolder<VE_TextureBased::Database_t> DatabaseH;
LOKI_SINGLETON_INSTANCE_DEFINITION( DatabaseH )

namespace VE_TextureBased
{
template<>
Database_t& Singleton<Database_t>::Instance()
{
    return Loki::SingletonHolder<Database_t>::Instance();
}
// Windows requires special measures for Singletons and DLLs.
#ifdef _WIN32
template class Singleton<Database_t>;
#endif
}
template class Loki::Singleton<VE_TextureBased::Database_t>;
