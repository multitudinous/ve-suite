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
 * File:          $RCSfile: TBToolBar.h TBToolBar.hSceneGraphBuilder.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
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

#include "VE_Conductor/Utilities/BaseDialog.h"

class wxArrayString;
class wxToolBar;
class wxComboBox;
class wxCheckBox;

class TextureBasedToolBar : public VE_Conductor::GUI_Utilities::BaseDialog
{
public:
   enum TBTOOLBAR_IDS
   {
      ACTIVE_SOLUTION ,///<Active scalar/vector ID
      SCALAR_ID,///<Scalar ID
      VECTOR_ID,///<Vector ID
      ROI_ID,///<Region of interest ID
      TRANSFER_FUNCS_ID,///<Transfer function ID
      TB_TOOLBAR,///<Toolbar ID
      BBOX_CHECK_BOX///<Bounding box check
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

   ///Activate the texture based visualization
   bool ActivateTextureVisualization();
   
protected:
   wxToolBar* _tbToolButtons;///<The toolbar buttons;
   wxComboBox* _solutionSelection;///<The list of active solutions.
   wxCheckBox* _bboxCheckBox;///<The check box to activate the Bounding Box
  
   wxArrayString _availableScalars;///<Scalar names
   wxArrayString _availableVectors;///<Vector names

   ///Build the toolbar.  
   void _buildGUI();

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
   
   ///Handle bounding box check box event
   ///\param event wxCommand event
   void _onBBoxCheck(wxCommandEvent& event);
   DECLARE_EVENT_TABLE()
};
#endif// TEXTURE_BASED_TOOL_BAR_H
