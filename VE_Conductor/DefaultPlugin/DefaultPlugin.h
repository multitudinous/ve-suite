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
#ifndef DefaultPlugin_H
#define DefaultPlugin_H

#include "VE_Conductor/GUIPlugin/UIPluginBase.h"
#include "VE_Installer/include/VEConfig.h"
#include <wx/image.h>

class VE_DEFAULT_PLUGIN_EXPORTS DefaultPlugin : public UIPluginBase
{
   DECLARE_DYNAMIC_CLASS( DefaultPlugin )

public:
   DefaultPlugin();
   virtual ~DefaultPlugin();

   virtual double GetVersion();
   //Return the version number of the module
   //virtual void DrawIcon(wxDC* dc);
   //This call return a window to be displayed on the framework
   virtual UIDialog* UI(wxWindow* parent);
   //This returns the UI dialog of the module
   virtual wxString GetConductorName();
   ///This returns the name of the module

   virtual wxString GetDesc();
   //This returns the description of the module, This should be a short description

public:
   long int height;
};

#endif
