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

#include "ves/conductor/util/CADMaterialEditMenu.h"
using namespace VE_Conductor::GUI_Utilities;
/////////////////////////////////////////
///Constructor                         //
/////////////////////////////////////////
CADMaterialEditMenu::CADMaterialEditMenu()
:wxMenu()
{
   Append(DIFFUSE_ID,
          _T("Diffuse..."),
          _T("Set Diffuse color of material"),
          wxITEM_NORMAL);

   Append(AMBIENT_ID,
          _T("Ambient..."),
          _T("Set Ambient color of material"),
          wxITEM_NORMAL);

   Append(SPECULAR_ID,
          _T("Specular..."),
          _T("Set Specular color of material"),
          wxITEM_NORMAL);

   Append(EMISSIVE_ID,
          _T("Emissive..."),
          _T("Set Emissive color of material"),
          wxITEM_NORMAL);
   InsertSeparator(4);

   Append(SHININESS_ID,
          _T("Shininess..."),
          _T("Set shininess of material"),
          wxITEM_NORMAL);
   Append(OPACITY_ID,
          _T("Opacity..."),
          _T("Set Opacity color of material"),
          wxITEM_NORMAL);
   InsertSeparator(7);

   Append(COLOR_MODE_ID,
          _T("Color Mode..."),
          _T("Set the color mode of this material"),
          wxITEM_NORMAL);
   Append(FACE_ID,
          _T("Face..."),
          _T("Set the face to apply this material to"),
          wxITEM_NORMAL);
}
/////////////////////////////
CADMaterialEditMenu::~CADMaterialEditMenu()
{
  
}
