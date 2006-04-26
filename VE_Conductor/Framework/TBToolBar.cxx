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
#include "VE_Conductor/Framework/TBToolBar.h"

#include <wx/sizer.h>
#include <wx/combobox.h>
#include <wx/toolbar.h>
#include <wx/image.h>
#include <wx/bitmap.h>
#include <wx/msgdlg.h>

#include "VE_Conductor/Framework/ROItb.xpm"

#include "VE_Conductor/Framework/cspline.xpm"
#include "VE_Conductor/Framework/scalartb.xpm"
#include "VE_Conductor/Framework/vectortb.xpm"
#include "VE_Conductor/Framework/scalartb_bw.xpm"
#include "VE_Installer/installer/installerImages/ve_ce_banner.xpm"
#include "VE_Installer/installer/installerImages/ve_xplorer_banner.xpm"

BEGIN_EVENT_TABLE(TextureBasedToolBar,wxDialog)
   EVT_TOOL_RANGE(SCALAR_ID,ACTIVE_SOLUTION,TextureBasedToolBar::_handleToolButtons)
END_EVENT_TABLE()

//////////////////////////////////////////////////////////////////
TextureBasedToolBar::TextureBasedToolBar(wxWindow* parent, int id,
                                   std::vector<std::string> scalarNames,
                                   std::vector<std::string> vectorNames)
:wxDialog((wxWindow *) parent, id, "Texture-Based ToolBar",wxDefaultPosition,wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX),wxString("Texture-Based ToolBar") )
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxHORIZONTAL);

   _updateAvailableSolutions(scalarNames, vectorNames);
   _buildToolBar();
   _updateSolutionList(_availableScalars);

   mainSizer->Add(_tbToolButtons,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(mainSizer);
   mainSizer->Fit(this);
}
///////////////////////////////////////////
TextureBasedToolBar::~TextureBasedToolBar()
{
   _availableScalars.clear();
   _availableVectors.clear();
}
////////////////////////////////////////////////////////////////////////////////////////
void TextureBasedToolBar::_updateAvailableSolutions(std::vector<std::string> scalarNames,
                                              std::vector<std::string> vectorNames)
{
   size_t nScalars = scalarNames.size();
   size_t nVectors = vectorNames.size();

   _availableScalars.clear();
   _availableVectors.clear();

   for(size_t i = 0; i < nScalars; i++)
   {
      _availableScalars.Add(scalarNames.at(i).c_str());
   }
   for(size_t i = 0; i < nVectors; i++)
   {
      _availableVectors.Add(vectorNames.at(i).c_str());
   }
}
////////////////////////////////////////////////////////////////////////////
void TextureBasedToolBar::_updateSolutionList(wxArrayString activeSolutions)
{
   _solutionSelection->Clear();
   for(size_t i = 0; i < activeSolutions.GetCount(); i++)
   {
      _solutionSelection->Append(activeSolutions[i]);
   }
   if(activeSolutions.GetCount())
   {
      _solutionSelection->SetValue(activeSolutions[0]);
   }
}
/////////////////////////////////////////
void TextureBasedToolBar::_buildToolBar()
{
   _tbToolButtons  = new wxToolBar(this,TB_TOOLBAR,wxDefaultPosition, wxDefaultSize,
                                wxTB_HORIZONTAL | wxNO_BORDER |wxTB_TEXT);
   _tbToolButtons->SetToolBitmapSize(wxSize(50,50));

   _solutionSelection = new wxComboBox(_tbToolButtons,ACTIVE_SOLUTION);
   _tbToolButtons->AddControl(_solutionSelection);

   wxImage scalarOnImage(scalartb_xpm);
   wxBitmap scalarOnBitmap(scalarOnImage);

   wxImage scalarOffImage(scalartb_bw_xpm);
   wxBitmap scalarOffBitmap(scalarOffImage);
   
   wxImage vectorOnImage(vectortb_xpm);
   wxBitmap vectorOnBitmap(vectorOnImage);

   wxImage ROIOnImage(ROItb_xpm);
   wxBitmap ROIOnBitmap(ROIOnImage);

   
   wxImage tfuncOnImage(cspline_xpm);
   wxBitmap tfuncOnBitmap(tfuncOnImage);
  
  

   _tbToolButtons->AddSeparator();
   _tbToolButtons->AddRadioTool(SCALAR_ID,_T("Scalars"),
                             scalarOnBitmap,wxNullBitmap,
                             _T("Scalar Data Tools") );

   _tbToolButtons->AddRadioTool(VECTOR_ID,_T("Vectors"),
                             vectorOnBitmap,wxNullBitmap,
                             _T("Vector Data Tools") );
  
   _tbToolButtons->AddSeparator();
   _tbToolButtons->AddTool(ROI_ID,_T("ROI"),
                        ROIOnBitmap,wxNullBitmap,
                        wxITEM_NORMAL,_T("Edit Region Of Interest") );
   _tbToolButtons->AddTool(TRANSFER_FUNCS_ID,_T("Transfer Functions"), 
                        tfuncOnBitmap,wxNullBitmap,
                        wxITEM_NORMAL,_T("Edit Transfer Functions") );

   _tbToolButtons->Realize();
   
}
///////////////////////////////////////////////////////////
void TextureBasedToolBar::SetVjObsPtr(VjObs_ptr xplorerCom)
{
    _vjObsPtr = xplorerCom;
}
/////////////////////////////////////////////
void TextureBasedToolBar::ClearInstructions()
{
    _instructions.clear();
}
///////////////////////////////////////////////////////////////////
void TextureBasedToolBar::_handleToolButtons(wxCommandEvent& event)
{
   switch(event.GetId())
   {
      case SCALAR_ID:
         wxMessageBox( "Scalar tools.", 
                        "Toolbar test", wxOK | wxICON_INFORMATION );
         _updateSolutionList(_availableScalars);
         event.Skip();
         
         break;
      case VECTOR_ID:
         wxMessageBox( "Vector tools.", 
                      "Toolbar test", wxOK | wxICON_INFORMATION );
         _updateSolutionList(_availableVectors);
         event.Skip();
         break;
      case TRANSFER_FUNCS_ID:
         wxMessageBox( "Transfer functions tools.", 
                        "Toolbar test", wxOK | wxICON_INFORMATION );
         break;
      case ROI_ID:
         wxMessageBox( "ROI tools.", 
                        "Toolbar test", wxOK | wxICON_INFORMATION );
         break;

   };
   //event.Skip();
}
//////////////////////////////////////////////////
void TextureBasedToolBar::_sendCommandsToXplorer()
{
}
