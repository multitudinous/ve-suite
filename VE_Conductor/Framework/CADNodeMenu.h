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
#ifndef CADNODE_MENU_H
#define CADNODE_MENU_H
/*!\file CADNodeMenu.h
  CADNodeMenu API
  */
/*!\class CADNodeMenu
 * GUI class to manipulate CADNode s and the corresponding osg::Node.
 */
#include <wx/menu.h>


class CADNodeMenu : public wxMenu{
public:
   enum GEOMETRY_MENU_IDS
   {
      GEOM_MENU_MOVE_NODE=1000,///<Move node in tree, menu ID.
      GEOM_NODE_ADD,///<Add node in tree, menu ID.
      GEOM_CLONE_ADD,///<Add cloned node to the tree, menu ID.
      GEOM_DELETE,///<Delete node from tree, menu ID.
      GEOM_PROPERTIES,//<Modify node properties,menu ID.
      GEOM_VEG_FILE_ADD,///Add a node from an existing VEG file,menu ID.
      GEOM_CAD_FILE_ADD,///Add a node from an existing CAD file, menu ID.
      GEOM_NODE_CREATE,///<Create a new tree from a file.
      GEOM_VEG_FILE_CREATE,///<Create a new tree from a file.
      GEOM_ASSEMBLY_CREATE,///<Create a new CADAssembly.
      GEOM_CAD_FILE_CREATE///<Create a new tree from a file.
   };

   ///Constructor
   CADNodeMenu();

   ///Destructor
   virtual ~CADNodeMenu();

   ///Turn on the global menus.
   ///\param onOff Turn on the global menus.
   void EnableGlobalMenus(bool onOff);
 
   ///Turn on the Create node menus.
   ///\param onOff Turn on the global menus.
   void EnableCreateMenu(bool onOff);
   
   ///Turn on the Assembly menus.
   ///\param onOff Turn on the global menus.
   void EnableAssemblyMenus(bool onOff);

   ///Turn on the Part menus.
   ///\param onOff Turn on the global menus.
   void EnablePartMenus(bool onOff);

   ///Turn on the clone menu.
   ///\param onOff Turn on the global menus.
   void EnableCloneMenu(bool onOff);

protected:
};
#endif// CADNODE_MENU_H
