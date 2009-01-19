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
* Date modified: $Date$
* Version:       $Rev$
* Author:        $Author$
* Id:            $Id$
* -----------------------------------------------------------------
*
*************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef _VES_OPEN_XML_MODEL_MODEL_PTR_H
#define _VES_OPEN_XML_MODEL_MODEL_PTR_H

#include <ves/util/PointerTypes.h>

/**
 * \file
 *
 * Include this file to get a forward declaration of the pointer type
 * VE_XML::VE_Model::ModelPtr.  To get the full
 * declaration of VE_XML::VE_Model::Model
 * VE_Open/XML/Model/Model.h must be included, too.
 */

namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
class Model;
/// Typedef for a SmartPtr type for the Model.
typedef ves::util::ClassPtrDef<Model>::type  ModelPtr;
typedef ves::util::SharedPtrDef<Model>::type ModelSharedPtr;
typedef ves::util::WeakPtrDef<Model>::type   ModelWeakPtr;
typedef ves::util::ScopedPtrDef<Model>::type ModelScopedPtr;
}
}
}
}

#endif
