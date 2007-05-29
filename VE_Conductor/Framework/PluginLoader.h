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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PLUGINLOADER_H
#define PLUGINLOADER_H
/*!\file PluginLoader.h
PluginLoader API
*/
/*!\class PluginLoader
* 
*/
#include <vector>
#include <wx/string.h>
#include <wx/msgdlg.h>

class wxClassInfo;

class UIPluginBase;

class PluginLoader 
{
public:
   PluginLoader();
   ~PluginLoader();

   bool LoadPlugins(wxString dir);
   //Load all the dlls in the given dir

   void RegisterPlugins();

   //Instantiate a instance of the plug_in. This instance is not used for any network composition but for information.
   void RegisterPlugin(wxClassInfo* info);


   // private:
   std::vector<UIPluginBase*> plugins;
   //Keep the list of the first intance of each plugin
   std::vector<wxClassInfo*> plugin_cls; 
   //The classinfo obj of the each plugin, will be use to generate more instances
};

#endif
