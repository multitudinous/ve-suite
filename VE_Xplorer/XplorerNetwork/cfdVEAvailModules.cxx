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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerNetwork/cfdVEAvailModules.h"
#include <iostream>

#include <fstream>
#include <sstream>

using namespace VE_Xplorer;

cfdVEAvail_Modules::cfdVEAvail_Modules( void )
{
   pl_loader = new cfdVEPluginLoader();
   LoadModules();
}

cfdVEAvail_Modules::~cfdVEAvail_Modules( void )
{
   delete pl_loader;
}

bool cfdVEAvail_Modules::LoadModules()
{
/*   char* path = "Plugins/";
   char* modelPath =  getenv("CFDHOSTTYPE");
   char* file = new char[100];
   
   strcpy( file, path );
   strcat( file, modelPath );

   wxString wxPath = file;
   pl_loader->LoadPlugins( wxPath );*/
   pl_loader->ScanAndLoad();
  
   //delete [] file;

   return true;
}

cfdVEPluginLoader* cfdVEAvail_Modules::GetLoader( void )
{
   return pl_loader;
}

