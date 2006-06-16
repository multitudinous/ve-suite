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

#ifndef BASE_DIALOG_H
#define BASE_DIALOG_H
#ifndef STAND_ALONE
#include "VE_Open/skel/VjObsC.h"
#endif
#include "VE_Installer/include/VEConfig.h"
#include <vector>
#include <string>
#include <wx/window.h>
#include <wx/sizer.h>
#include <wx/dialog.h>

namespace VE_XML
{
   class DataValuePair;
}
namespace VE_Conductor
{
namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS BaseDialog : public wxDialog
{
public:
   ///Constructor
   ///\param parent The parent window.
   ///\param id The ID for the dialog.
   ///\param title The title to display on the dialog.
   BaseDialog( wxWindow* parent, int id,std::string title);

   ///Destructor
   virtual ~BaseDialog();

#ifndef STAND_ALONE
   ///Set the current vjObjs ptr for data passing.
   ///\param xplorerCom The communication interface w/ xplorer.
   void SetVjObsPtr(VjObs_ptr xplorerCom);
#endif

   ///Clear out the current queue of instructions.
   void ClearInstructions();
protected:
   ///Override this method in derived classes to add controls to the dialog.
   virtual void _buildGUI()=0;

   ///Add an OK button to the dialog
   ///\param buttonRowSizer The sizer to add the button to
   void _addOKButton(wxSizer* buttonRowSizer);
   
   ///Add a Cancel button to the dialog
   ///\param buttonRowSizer The sizer to add the button to
   void _addCancelButton(wxSizer* buttonRowSizer);

#ifndef STAND_ALONE
   VjObs_ptr _vjObsPtr;///<The VjObj ptr.
   
   ///Send the Command back to VE-Xplorer.
   void _sendCommandsToXplorer();
#endif
   std::string _commandName;///<The name of the Command
   std::vector<VE_XML::DataValuePair*> _instructions;///<The DataValuePair s for the current command.
};
}
}
#endif//BASE_DIALOG_H

