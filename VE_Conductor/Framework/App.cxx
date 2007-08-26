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
#include "VE_Conductor/Framework/Frame.h"
#include "VE_Conductor/Framework/App.h"

#include <wx/dirdlg.h>
#include <wx/intl.h>
#include <wx/filename.h>
#include <wx/string.h>

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

#include <iostream>

IMPLEMENT_APP(ConductorApp);

XERCES_CPP_NAMESPACE_USE

bool ConductorApp::OnInit()
{
   try
   {
      XMLPlatformUtils::Initialize();
   }
   catch(const XMLException &toCatch)
   {
      std::cerr << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode(toCatch.getMessage()) << std::endl;
      return false;
   }

   SetAppName(_("VE-Conductor"));
   // this->SetIcon( wxIcon( ve_xplorer_banner_xpm ) );
   mainFrame= new AppFrame(NULL, 1023, _("VE-Conductor"));

   // Problem with generic wxNotebook implementation whereby it doesn't size
   // properly unless you set the size again

   #if defined(__WIN16__) || defined(__WXMOTIF__)
   int width, height;
   mainFrame->GetSize(& width, & height);
   mainFrame->SetSize(-1, -1, width, height);
   #endif

   // Now launch the main ui
   mainFrame->Show(true);
   SetTopWindow(mainFrame);
   return true;
}

int ConductorApp::OnExit()
{
   delete wxConfigBase::Set((wxConfigBase *) NULL);
   XMLPlatformUtils::Terminate();
   return 0;
}
