/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CADMATERIAL_EDIT_MENU_H
#define CADMATERIAL_EDIT_MENU_H
/*!\file CADMaterialEditMenu.h
  CADMaterialEditMenu API
  */
/*!\class CADMaterialEditMenu
 * GUI class to edit CADMaterial s .
 */
#include <wx/menu.h>


class CADMaterialEditMenu : public wxMenu{
public:
   enum MATERIAL_EDIT_MENU_IDS
   {
      DIFFUSE_ID,   
      AMBIENT_ID,   
      SPECULAR_ID,   
      EMISSIVE_ID,   
      SHININESS_ID,   
      OPACITY_ID,
      FACE_ID,
      COLOR_MODE_ID
   };

   ///Constructor
   CADMaterialEditMenu();

   ///Destructor
   virtual ~CADMaterialEditMenu();


protected:
};
#endif// CADMATERIAL_EDIT_MENU_H
