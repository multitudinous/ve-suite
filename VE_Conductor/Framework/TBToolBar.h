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

#ifndef TEXTURE_BASED_TOOL_BAR_H
#define TEXTURE_BASED_TOOL_BAR_H

#ifndef STAND_ALONE
#include "VE_Open/skel/VjObsC.h"
#endif

namespace VE_XML 
{
   class DataValuePair;
}

#include <wx/dialog.h>
#include <vector>
#include <string>

class wxArrayString;
class wxToolBar;
class wxComboBox;

class TextureBasedToolBar : public wxDialog
{
public:
   enum TBTOOLBAR_IDS
   {
      ACTIVE_SOLUTION=99999 ,///<Active scalar/vector ID
      SCALAR_ID,///<Scalar ID
      VECTOR_ID,///<Vector ID
      ROI_ID,///<Region of interest ID
      TRANSFER_FUNCS_ID,///<Transfer function ID
      TB_TOOLBAR///<Toolbar ID
   };
   ///Constructor
   TextureBasedToolBar(wxWindow* parent, int id/*,
                     wxArrayString scalarNames,
                     wxArrayString vectorNames*/);

   ///Destructor
   virtual ~TextureBasedToolBar();

   ///Set the scalars
   void SetScalars(wxArrayString scalarNames);
   
   ///Set the vectors
   void SetVectors(wxArrayString vectorNames);

#ifndef STAND_ALONE
   ///Set the current vjObjs ptr for data passing.
   ///\param xplorerCom The communication interface w/ xplorer.
   void SetVjObsPtr(VjObs_ptr xplorerCom);
#endif

   ///Clear out the current queue of instructions.
   void ClearInstructions();
   
   
protected:
   wxToolBar* _tbToolButtons;///<The toolbar buttons;
   wxComboBox* _solutionSelection;///<The list of active solutions.
   std::vector<VE_XML::DataValuePair*> _instructions;///<The DataValuePair s for the current command.
   
   wxArrayString _availableScalars;///<Scalar names
   wxArrayString _availableVectors;///<Vector names

#ifndef STAND_ALONE
   VjObs_ptr _vjObsPtr;///<The VjObj ptr.

   ///Send the Command back to VE-Xplorer.
   void _sendCommandsToXplorer();
#endif 
   ///Build the toolbar.  
   void _buildToolBar();

   ///update the displayed available solutions
   ///\param activeSolutions The active solutions names
   void _updateSolutionList(wxArrayString activeSolutions);

   ///update the list of scalars and vectors
   ///\param scalarNames Scalar names
   ///\param vectorNames Vector names
   void _updateAvailableSolutions(wxArrayString scalarNames,
                               wxArrayString vectorNames);
   
   ///Handle tool button presses
   ///\param event wxCommand event
   void _handleToolButtons(wxCommandEvent& event);
   DECLARE_EVENT_TABLE()
};
#endif// TEXTURE_BASED_TOOL_BAR_H
