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
#include "VE_Conductor/Utilities/UI_TransientDialog.h"

#include "VE_Conductor/Utilities/TBToolBar.h"

#include <wx/sizer.h>
#include <wx/combobox.h>
#include <wx/toolbar.h>
#include <wx/image.h>
#include <wx/bitmap.h>
#include <wx/msgdlg.h>
#include <wx/filename.h>
#include <wx/checkbox.h>
#include <wx/button.h>

#include "VE_Conductor/xpm/ROItb.xpm"

#include "VE_Conductor/xpm/cspline.xpm"
#include "VE_Conductor/xpm/scalartb.xpm"
#include "VE_Conductor/xpm/vectortb.xpm"
#include "VE_Conductor/xpm/scalartb_bw.xpm"

#include "VE_Conductor/Utilities/ROIDialog.h"
#include "VE_Conductor/Utilities/ScalarToolsDlg.h"
#include "VE_Conductor/Utilities/TransferFunctionDlg.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include <iostream>
BEGIN_EVENT_TABLE(TextureBasedToolBar,wxDialog)
   EVT_TOOL_RANGE(SCALAR_ID,TRANSFER_FUNCS_ID,TextureBasedToolBar::_handleToolButtons)
   //EVT_CHECKBOX(BBOX_CHECK_BOX,TextureBasedToolBar::_onBBoxCheck)
   EVT_BUTTON(TRANSIENT_BUTTON,TextureBasedToolBar::_onTransient)
END_EVENT_TABLE()

using namespace VE_Conductor::GUI_Utilities;
//////////////////////////////////////////////////////////////////
TextureBasedToolBar::TextureBasedToolBar(wxWindow* parent, int id)
:VE_Conductor::GUI_Utilities::BaseDialog(parent,id,"Texture-Based Tools")
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);

   _buildGUI();
   _transientControls = 0;

   mainSizer->Add(_tbToolButtons,2,wxALIGN_CENTER);
   wxBoxSizer* buttonRowSizer = new wxBoxSizer(wxHORIZONTAL);
   //_bboxCheckBox = new wxCheckBox(this,BBOX_CHECK_BOX,"Display Bounds");
   //_bboxCheckBox->SetValue(true);

   //buttonRowSizer->Add(_bboxCheckBox,0,wxALIGN_CENTER);
   buttonRowSizer->Add(new wxButton(this,TRANSIENT_BUTTON,_T("Transient")),0,wxALIGN_CENTER);
   _addOKButton(buttonRowSizer);
   
   mainSizer->Add(buttonRowSizer,1,wxALIGN_CENTER);

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
//////////////////////////////////////////////////////////
void TextureBasedToolBar::SetSubDialogSize(wxRect subSize)
{
   _subDialogSize = subSize;
}
////////////////////////////////////////////////////////////////////////////////////////
void TextureBasedToolBar::_updateAvailableSolutions(wxArrayString scalarNames,
                                              wxArrayString vectorNames)
{
}
////////////////////////////////////////////////////////////////
void TextureBasedToolBar::SetScalars(wxArrayString scalarNames)
{
   size_t nScalars = scalarNames.Count();
   _availableScalars.Clear();

   
   for(size_t i = 0; i < nScalars; i++)
   {
      if(scalarNames[i] != wxString("",wxConvUTF8))
      {
         wxFileName fName(scalarNames[i]);
         _availableScalars.Add( wxString(fName.GetName(),wxConvUTF8));
      }
      else
      {
        std::cout<<"Scalar..."<<std::endl;
        std::cout<<scalarNames[i]<<std::endl;
      }
   }
}
///////////////////////////////////////////////////////////////
void TextureBasedToolBar::SetVectors(wxArrayString vectorNames)
{
   size_t nVectors = vectorNames.Count();
   _availableVectors.Clear();
   
 
   for(size_t i = 0; i < nVectors; i++)
   {
      if(vectorNames[i] != wxString("",wxConvUTF8))
      {
         wxFileName fName(vectorNames[i]);
         _availableVectors.Add( wxString(fName.GetName(), wxConvUTF8));
      }
      else
      {
        std::cout<<"Vector..."<<std::endl;
        std::cout<<vectorNames[i]<<std::endl;
      }
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
void TextureBasedToolBar::_buildGUI()
{
   _tbToolButtons  = new wxToolBar(this,TB_TOOLBAR,wxDefaultPosition, wxDefaultSize,
                                wxTB_HORIZONTAL | wxNO_BORDER |wxTB_TEXT);
   _tbToolButtons->SetToolBitmapSize(wxSize(50,50));

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
   _tbToolButtons->AddTool(SCALAR_ID,_T("Scalars"),
                             scalarOnBitmap,wxNullBitmap,
                             wxITEM_NORMAL,
                             _T("Scalar Data Tools") );
   
   _tbToolButtons->AddTool(VECTOR_ID,_T("Vectors"),
                             vectorOnBitmap,wxNullBitmap,
                             wxITEM_NORMAL,
                             _T("Vector Data Tools") );
   _tbToolButtons->AddSeparator();
   _tbToolButtons->AddTool(ROI_ID,_T("ROI"),
                        ROIOnBitmap,wxNullBitmap,
                        wxITEM_NORMAL,_T("Edit Region Of Interest") );
   _tbToolButtons->AddTool(TRANSFER_FUNCS_ID,_T("Transfer Functions"), 
                        tfuncOnBitmap,wxNullBitmap,
                        wxITEM_NORMAL,_T("Edit Transfer Functions") );

   _tbToolButtons->Realize();

   ///the sub dialogs
   ///ROI Dialog
   _roiDlg = new VE_Conductor::GUI_Utilities::ROIDialog(this,-1, "Volume Clipping Bounds" );
   //_roiDlg->SetSize(GetRect().x, GetRect().y, -1, -1, wxSIZE_USE_EXISTING);
   _scalarToolsDlg = new ScalarToolsDialog(this,-1, "Scalar Tools" );
   _transferFunctionDlg = new TransferFunctionDialog(this,-1,"Transfer Functions");
   
}
/////////////////////////////////////////////////////////////
/*void TextureBasedToolBar::_onBBoxCheck(wxCommandEvent& event)
{
   ClearInstructions();
   _commandName = "TB_BBOX_DISPLAY";
   
   VE_XML::DataValuePair* showBBox = new VE_XML::DataValuePair();
   showBBox->SetDataType("UNSIGNED INT");
   showBBox->SetDataName(std::string("BBox Flag"));
   unsigned int value = 0;
   if(_bboxCheckBox->GetValue())
   {
      value = 1;
   }
   else
   {
      value = 0;
   }   
   showBBox->SetDataValue(value);
   
   _instructions.push_back(showBBox);
   _sendCommandsToXplorer();
   ClearInstructions();
}*/
//////////////////////////////////////////////////////////////
void TextureBasedToolBar::_onTransient(wxCommandEvent& event)
{
   if(!_transientControls)
   {
      _transientControls = new UI_TransientDialog(0,this,-1);
   }
   _transientControls->SetVjObsPtr(_vjObsPtr);
   _transientControls->SetSize(GetRect());
   _transientControls->ShowModal();
}
///////////////////////////////////////////////////////////////////
void TextureBasedToolBar::_handleToolButtons(wxCommandEvent& event)
{
   switch(event.GetId())
   {
      case SCALAR_ID:
         {
            if(_availableScalars.GetCount())
            {
               //_scalarToolsDlg->SetVjObsPtr(_vjObsPtr);
               _scalarToolsDlg->UpdateScalarList(_availableScalars);
               _scalarToolsDlg->SetSize(_subDialogSize);
               _scalarToolsDlg->CentreOnParent();
               if(_scalarToolsDlg->ShowModal() == wxID_OK)
               {
                  ;
               }
            }
            else
            {
               wxMessageBox( _("ERROR!"), 
                            _("No scalar data available!!"), wxOK | wxICON_ERROR);
            
            }
         }
         
         break;
      case VECTOR_ID:
         wxMessageBox( _("Unavailable!!"),_("Vector tools"), 
                       wxOK | wxICON_INFORMATION );
         break;
      case TRANSFER_FUNCS_ID:
         /*wxMessageBox( _("Transfer functions tools."), 
                        _("Unavailable!!"), wxOK | wxICON_INFORMATION );*/
		  //_transferFunctionDlg->SetVjObsPtr(_vjObsPtr);
		  _transferFunctionDlg->SetSize(_subDialogSize);
        _transferFunctionDlg->CentreOnParent();
		  if(_transferFunctionDlg->ShowModal()== wxID_OK)
		  {
			  ;
		  }
         break;
      case ROI_ID:
         {
            //_roiDlg->SetVjObsPtr(_vjObsPtr);
            _roiDlg->SetSize(_subDialogSize);
            _roiDlg->CentreOnParent();
            if(_roiDlg->ShowModal() == wxID_OK)
            {
               ;
            }
         }
         break;

   };
}
/////////////////////////////////////////////////////////
bool TextureBasedToolBar::ActivateTextureVisualization()
{
   ClearInstructions();
   _commandName = "TB_ACTIVATE";
   
   if(_availableScalars.GetCount())
   {
      VE_XML::DataValuePair* activateCommand = new VE_XML::DataValuePair();
      activateCommand->SetDataType("STRING");
      activateCommand->SetDataName(std::string("Active Scalar"));
      activateCommand->SetDataString( ConvertUnicode( _availableScalars[0].GetData() ) );
      _instructions.push_back(activateCommand);
      _sendCommandsToXplorer();
   }
   else if(_availableVectors.GetCount())
   {
      VE_XML::DataValuePair* activateCommand = new VE_XML::DataValuePair();
      activateCommand->SetDataType("STRING");
      activateCommand->SetDataName(std::string("Active Vector"));
      activateCommand->SetDataString( ConvertUnicode(_availableVectors[0].GetData()));
      _instructions.push_back(activateCommand);
      _sendCommandsToXplorer();
   }
   else
   {
      wxMessageBox( _("No texture-based datasets available!!"),_("ERROR"), 
                    wxOK | wxICON_ERROR );
      _tbToolButtons->Enable(false);
      return false;

   }
   _tbToolButtons->Enable(true);
   ClearInstructions();
   return true;
}
