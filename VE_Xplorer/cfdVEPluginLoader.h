
/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdVEPluginLoader.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef CFD_PLUGINLOADER_H
#define CFD_PLUGINLOADER_H
#include <wx/wx.h>
//#include <wx/dynlib.h>
#include <wx/dynload.h>

#include <vector>

namespace VE_Xplorer
{
   class cfdVEBaseClass;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdVEPluginLoader : public wxObject
   {
      public:

         cfdVEPluginLoader();
         ~cfdVEPluginLoader();

         bool LoadPlugins(wxString dir);
         //Load all the dlls in the given dir

         void RegisterPlugins();

         //Instantiate an instance of the plug_in. This instance is not used for any network composition but for information.
         void RegisterPlugin(wxClassInfo* info);

         //char* GetPluginName(int);

         int GetNumberOfPlugins();

         cfdVEBaseClass* CreateObject( char* ); 

      //private:
         std::vector<cfdVEBaseClass*> plugins;
         //Keep the list of the first intance of each plugin
         std::vector<const wxClassInfo*> plugin_cls; 
         //The classinfo obj of the each plugin, will be use to generate more instances

         std::vector<wxPluginLibrary *> libs;
   };
}
#endif
