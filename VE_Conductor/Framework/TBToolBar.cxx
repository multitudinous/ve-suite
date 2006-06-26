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
 * File:          $RCSfile: TBToolBar.cxx TBToolBar.cxxSceneGraphBuilder.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
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
#include <wx/filename.h>

#include "VE_Conductor/xpm/ROItb.xpm"

#include "VE_Conductor/xpm/cspline.xpm"
#include "VE_Conductor/xpm/scalartb.xpm"
#include "VE_Conductor/xpm/vectortb.xpm"
#include "VE_Conductor/xpm/scalartb_bw.xpm"

#include "VE_Conductor/Utilities/ROIDialog.h"
#include "VE_Conductor/Framework/ScalarToolsDlg.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include "VE_Installer/installer/installerImages/ve_ce_banner.xpm"
#include "VE_Installer/installer/installerImages/ve_xplorer_banner.xpm"
#include <iostream>
BEGIN_EVENT_TABLE(TextureBasedToolBar,wxDialog)
   EVT_TOOL_RANGE(SCALAR_ID,TRANSFER_FUNCS_ID,TextureBasedToolBar::_handleToolButtons)
END_EVENT_TABLE()

using namespace VE_Conductor::GUI_Utilities;
//////////////////////////////////////////////////////////////////
TextureBasedToolBar::TextureBasedToolBar(wxWindow* parent, int id)
:VE_Conductor::GUI_Utilities::BaseDialog(parent,id,"Texture-Based Tools")
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);

   //_availableScalars.Add("No data available");
   //_buildToolBar();
   _buildGUI();
   /*if(!_availableScalars.Count())
   {
      if(!_availableVectors.Count())
      {
         _availableScalars.Add("No data available");
         _updateSolutionList(_availableScalars);
      }
      else
      {
         _updateSolutionList(_availableVectors);
      }
   }
   else 
   {
      _updateSolutionList(_availableScalars);
   }*/

   mainSizer->Add(_tbToolButtons,1,wxALIGN_CENTER_HORIZONTAL);
   wxBoxSizer* buttonRowSizer = new wxBoxSizer(wxHORIZONTAL);
   _addOKButton(buttonRowSizer);
   
   mainSizer->Add(buttonRowSizer,0,wxALIGN_CENTER_HORIZONTAL);

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
      if(scalarNames[i] != wxString(""))
      {
         wxFileName fName(scalarNames[i]);
         _availableScalars.Add(fName.GetName());
      }
      else
      {
        std::cout<<"Scalar..."<<std::endl;
        std::cout<<scalarNames[i]<<std::endl;
      }
   }
  // _updateSolutionList(_availableScalars);
}
///////////////////////////////////////////////////////////////
void TextureBasedToolBar::SetVectors(wxArrayString vectorNames)
{
   size_t nVectors = vectorNames.Count();
   _availableVectors.Clear();
   
 
   for(size_t i = 0; i < nVectors; i++)
   {
      if(vectorNames[i] != wxString(""))
      {
         wxFileName fName(vectorNames[i]);
         _availableVectors.Add(fName.GetName());
      }
      else
      {
        std::cout<<"Vector..."<<std::endl;
        std::cout<<vectorNames[i]<<std::endl;
      }
   }
   //_updateSolutionList(_availableVectors);
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

   /*_solutionSelection = new wxComboBox(this,ACTIVE_SOLUTION,wxEmptyString, wxDefaultPosition, wxSize(150,wxDefaultCoord) );
   _updateSolutionList(_availableScalars);
   _tbToolButtons->AddControl(_solutionSelection);
*/
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
               ScalarToolsDialog stDlg(this,-1,"Scalar Tools");
               stDlg.SetVjObsPtr(_vjObsPtr);
               stDlg.UpdateScalarList(_availableScalars);
               if(stDlg.ShowModal() == wxID_OK)
               {
               }
            }
            else
            {
               wxMessageBox( "ERROR!", 
                            "No scalar data available!!", wxOK | wxICON_ERROR);
            
            }
         }
         //_updateSolutionList(_availableScalars);
         //event.Skip();
         
         break;
      case VECTOR_ID:
         wxMessageBox( "Vector tools.", 
                      "Toolbar test", wxOK | wxICON_INFORMATION );
         //_updateSolutionList(_availableVectors);
         //event.Skip();
         break;
      case TRANSFER_FUNCS_ID:
         wxMessageBox( "Transfer functions tools.", 
                        "Toolbar test", wxOK | wxICON_INFORMATION );
         break;
      case ROI_ID:
         {
           ROIDialog roiDlg(this,-1,"Volume Clipping Bounds");
         /*int displayWidth, displayHeight = 0;
         ::wxDisplaySize(&displayWidth,&displayHeight);
  
         wxRect bbox = GetRect();

         int width,height = 0;
         GetSize(&width,&height);
         roiDlg.SetSize(wxRect( 2*displayWidth/3, bbox.GetBottomRight().y, 
                       displayWidth/3, .5*(displayHeight-bbox.GetBottomRight().y)));*/
            roiDlg.SetVjObsPtr(_vjObsPtr);
            if(roiDlg.ShowModal() == wxID_OK)
            {
               wxMessageBox( "ROI tools.", 
                       "Toolbar test", wxOK | wxICON_INFORMATION );
            }
         }
         break;

   };
   //_tbToolButtons->ToggleTool(event.GetId(),false);
   //event.Skip();
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
      activateCommand->SetDataString(_availableScalars[0].GetData());
      _instructions.push_back(activateCommand);
      _sendCommandsToXplorer();
   }
   else if(_availableVectors.GetCount())
   {
      VE_XML::DataValuePair* activateCommand = new VE_XML::DataValuePair();
      activateCommand->SetDataType("STRING");
      activateCommand->SetDataName(std::string("Active Vector"));
      activateCommand->SetDataString(_availableVectors[0].GetData());
      _instructions.push_back(activateCommand);
      _sendCommandsToXplorer();
   }
   else
   {
      wxMessageBox( "ERROR.", 
                   "No texture-based datasets available!!", wxOK | wxICON_ERROR );
      _tbToolButtons->Enable(false);
      return false;

   }
   ClearInstructions();
   return true;
}
