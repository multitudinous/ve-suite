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
 * File:          $RCSfile: GlobalParamDialog.h,v $
 * Date modified: $Date: 2006-03-23 17:47:31 -0600 (Thu, 23 Mar 2006) $
 * Version:       $Rev: 3957 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/isosurfaces.h"
#include "VE_Conductor/Framework/vistab.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/choicdlg.h>
#include <wx/msgdlg.h>
#include <iostream>
BEGIN_EVENT_TABLE( TextureBasedIsosurfaceDlg, wxDialog )
   EVT_SLIDER(ISOSURFACE_SLIDER, TextureBasedIsosurfaceDlg::_onUpdateIsoSurface)
   EVT_BUTTON(ADVANCED_BUTTON, TextureBasedIsosurfaceDlg::_onAdvanced)
END_EVENT_TABLE()
using namespace VE_Conductor::GUI_Utilities;
//////////////////////////////////////////////////////////
TextureBasedIsosurfaceDlg::TextureBasedIsosurfaceDlg( wxWindow* parent, int id,std::string title )
:BaseDialog(parent,id,title)
{
   _buildGUI();
   wxSize displaySize = ::wxGetDisplaySize();
   int tempH = displaySize.GetHeight()-480;
   wxRect dialogPosition( displaySize.GetWidth()-427, displaySize.GetHeight()-tempH, 427, tempH );
   this->SetSize( dialogPosition );
}
//////////////////////////////////
void TextureBasedIsosurfaceDlg::_buildGUI()
{    
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
   SetSizer(mainSizer);

   /*
   wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(this, wxID_ANY, _T("Isosurface Controls"));
   wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
   itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);*/

   wxStaticBox* scalarNames = new wxStaticBox(this, -1, wxT("Active Scalar"));
   wxStaticBoxSizer* scalarNameSizer = new wxStaticBoxSizer(scalarNames, wxVERTICAL);
   _availableScalars = new wxComboBox(this,availableScalars);//,wxEmptyString, wxDefaultPosition, wxSize(150,wxDefaultCoord) );
   scalarNameSizer->Add(_availableScalars,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   mainSizer->Add(scalarNameSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   wxStaticText* itemStaticText6 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Isosurface"), wxDefaultPosition, wxDefaultSize, 0 );
   mainSizer->Add(itemStaticText6, 0, wxALIGN_LEFT|wxALL|wxADJUST_MINSIZE, 5);

   _isoSurfaceSlider = new wxSlider( this, ISOSURFACE_SLIDER, 0, 0, 100, wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL|wxSL_LABELS );
   mainSizer->Add(_isoSurfaceSlider, 0, wxGROW|wxALL, 5);

   _advancedButton = new wxButton( this, ADVANCED_ISOSURFACE_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
   mainSizer->Add(_advancedButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   wxBoxSizer* buttonRowSizer = new wxBoxSizer(wxHORIZONTAL);
   _addOKButton(buttonRowSizer);
   _addCloseButton(buttonRowSizer);
   
   mainSizer->Add(buttonRowSizer,1,wxALIGN_CENTER|wxEXPAND);
   
   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(mainSizer);
   mainSizer->Fit(dynamic_cast<BaseDialog*>(this));
}
//////////////////////////////////////////////////////////////////////////
void TextureBasedToolBar::SetAvailableScalars(wxArrayString availableScalars)
{
   _availableScalars->Clear();
   for(size_t i = 0; i < activeSolutions.GetCount(); i++)
   {
      _availableScalars->Append(activeSolutions[i]);
   }
   if(activeSolutions.GetCount())
   {
      _availableScalars->SetValue(activeSolutions[0]);
   }
}
/////////////////////////////////////////////////////////////////////////
void TextureBasedIsosurfaceDlg::SetActiveScalar(std::string activeScalar)
{
   _activeScalar = activeScalar;
}
////////////////////////////////////////////////////////////////
void TextureBasedIsosurfaceDlg::SetAvailableScalars(wxArrayString scalarNames)
{
   _scalarNames.Clear();
   for(size_t i = 0; i < scalarNames.Count(); i++)
   {
      _scalarNames.Add(scalarNames[i]);
   }
}
////////////////////////////////////////////////////////////////////////
void TextureBasedIsosurfaceDlg::_onUpdateIsoSurface( wxCommandEvent& WXUNUSED(event) )
{
   VE_XML::Command* newCommand = new VE_XML::Command();
   newCommand->SetCommandName("UPDATE_ISOSURFACE_SETTINGS");
   
   VE_XML::DataValuePair* isosurfaceValue = new VE_XML::DataValuePair();
   isosurfaceValue->SetData("Iso-Surface Value",static_cast<double>((_isoSurfaceSlider->GetValue())));
   newCommand->AddDataValuePair(isosurfaceValue);

   VE_XML::DataValuePair* colorByScalar = new VE_XML::DataValuePair();
   colorByScalar->SetData("Color By Scalar",_colorByScalarName);
   newCommand->AddDataValuePair(colorByScalar);

   VE_XML::DataValuePair* nearestPrecomputed = new VE_XML::DataValuePair();
   nearestPrecomputed->SetDataName("Use Nearest Precomputed");
   nearestPrecomputed->SetDataType("UNSIGNED INT");
   if(_useNearestPreComputedCheckBox->GetValue())
   {
      nearestPrecomputed->SetDataValue(static_cast<unsigned int>(1));
   }
   else
   {
      nearestPrecomputed->SetDataValue(static_cast<unsigned int>(0));
   }
   newCommand->AddDataValuePair(nearestPrecomputed);

   try
   {
      dynamic_cast<Vistab*>(GetParent())->SendUpdatedSettingsToXplorer(newCommand);
   }
   catch(...)
   {
      {
         wxMessageBox( "Invalid Parent","Communication Failure", 
            wxOK | wxICON_INFORMATION );
         if(newCommand)
         {
            delete newCommand;
            newCommand = 0;
         }
      }
   }
}
//////////////////////////////////////////////////////
void Isosurfaces::_onAdvanced( wxCommandEvent& WXUNUSED(event) )
{
   int selectionIndex = 0;
   for(size_t i = 0; i < _scalarNames.Count(); i++)
   {
      if(!_scalarNames[i].Cmp(_colorByScalarName.c_str()) )
      {
         selectionIndex = i;
         break;
      }
   }
   wxSingleChoiceDialog scalarSelector(this, _T("Select Scalar to color isosurface by."), _T("Color by Scalar"),
                                   _scalarNames);
   int displayWidth, displayHeight = 0;
   ::wxDisplaySize(&displayWidth,&displayHeight);
  
   wxRect bbox = GetRect();

   int width,height = 0;
   GetSize(&width,&height);
   scalarSelector.SetSize(wxRect( 2*displayWidth/3, bbox.GetBottomRight().y, 
                        width, height));
   scalarSelector.SetSelection(selectionIndex);
   if (scalarSelector.ShowModal() == wxID_OK)
   {
      _colorByScalarName = scalarSelector.GetStringSelection();
   }
}

