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
#include "VE_Conductor/Framework/polydata.h"
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

#include <wx/stattext.h>
#include <wx/statbox.h>
#include <iostream>
///////////////////////////
BEGIN_EVENT_TABLE( Polydata, wxDialog )
////@begin polydata event table entries
   EVT_RADIOBUTTON      (POLYDATA_RBUTTON,          Polydata::_onPolydata)
   EVT_CHECKBOX         (PRECOMPUTED_POLY_CHK,      Polydata::_onPrecomputedPolydata)
   EVT_SLIDER           (POLYDATA_PLANE_SLIDER,     Polydata::_onPolydataPlane)
   EVT_BUTTON           (ADD_POLYDATA_BUTTON,       Polydata::_onAddPolydata)
   EVT_BUTTON           (ADVANCED_POLYDATA_BUTTON,  Polydata::_onAdvanced)
////@end polydata event table entries
END_EVENT_TABLE()
Polydata::Polydata( )
{

}
//////////////////////////////////////////////////////////
Polydata::Polydata( wxWindow* parent, wxWindowID id,
                       const wxString& caption, 
                       const wxPoint& pos, 
                       const wxSize& size, long style )
{
   Create(parent, id, caption, pos, size, style);
   wxSize displaySize = ::wxGetDisplaySize();
   int tempH = displaySize.GetHeight()-480;
   wxRect dialogPosition( displaySize.GetWidth()-427, displaySize.GetHeight()-tempH, 427, tempH );
   this->SetSize( dialogPosition );
}
//////////////////////////////////////////////////////////
bool Polydata::Create( wxWindow* parent, wxWindowID id, 
                       const wxString& caption, 
                       const wxPoint& pos, 
                       const wxSize& size, long style )
{
   _useNearestPreComputedCheckBox = 0;
   _polydataSlider = 0;
   _advancedButton = 0;
   _computeButton = 0;

   SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
   wxDialog::Create( parent, id, caption, pos, size, style );

   CreateControls();
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
   Centre();

   return true;
}
//////////////////////////////////
void Polydata::CreateControls()
{    
    Polydata* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Polydata Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

   
    _useNearestPreComputedCheckBox = new wxCheckBox( itemDialog1, PRECOMPUTED_POLY_CHK, _T("Use Nearest Precomputed Polydata"), wxDefaultPosition, wxDefaultSize, 0 );
    _useNearestPreComputedCheckBox->SetValue(false);
    itemStaticBoxSizer3->Add(_useNearestPreComputedCheckBox, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText6 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Polydata"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText6, 0, wxALIGN_LEFT|wxALL|wxADJUST_MINSIZE, 5);

    _polydataSlider = new wxSlider( itemDialog1, POLYDATA_PLANE_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(_polydataSlider, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer8 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer8, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    _computeButton = new wxButton( itemDialog1, ADD_POLYDATA_BUTTON, _T("Update"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add(_computeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    _advancedButton = new wxButton( itemDialog1, ADVANCED_POLYDATA_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add(_advancedButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add(_closeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
}
///////////////////////////////////////////////////////////
void Polydata::SetActiveScalar(std::string activeScalar)
{
   _activeScalar = activeScalar;
}
////////////////////////////////////////////////////////////////
void Polydata::SetAvailableScalars(wxArrayString scalarNames)
{
   _scalarNames.Clear();
   for(size_t i = 0; i < scalarNames.Count(); i++)
   {
      _scalarNames.Add(scalarNames[i]);
   }
}
////////////////////////////////
bool Polydata::ShowToolTips()
{
    return true;
}
////////////////////////////////////////////////////////////////
wxBitmap Polydata::GetBitmapResource( const wxString& name )
{
   wxUnusedVar(name);
   return wxNullBitmap;
}
////////////////////////////////////////////////////////////
wxIcon Polydata::GetIconResource( const wxString& name )
{
   wxUnusedVar(name);
   return wxNullIcon;
}
/////////////////////////////////////////////////////////
void Polydata::_onPolydata( wxCommandEvent& WXUNUSED(event) )
{

}
////////////////////////////////////////////////////////////////////
void Polydata::_onPrecomputedPolydata( wxCommandEvent& WXUNUSED(event) )
{

}
/////////////////////////////////////////////////////////////
void Polydata::_onPolydataPlane( wxCommandEvent& WXUNUSED(event) )
{
}
///////////////////////////////////////////////////////////
void Polydata::_onAddPolydata( wxCommandEvent& WXUNUSED(event) )
{
   VE_XML::Command* newCommand = new VE_XML::Command();
   newCommand->SetCommandName("UPDATE_POLYDATA_SETTINGS");
   
   VE_XML::DataValuePair* polydataValue = new VE_XML::DataValuePair();
   polydataValue->SetData("Polydata Value",static_cast<double>((_polydataSlider->GetValue())));
   newCommand->AddDataValuePair(polydataValue);

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
void Polydata::_onAdvanced( wxCommandEvent& WXUNUSED(event) )
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
   wxSingleChoiceDialog scalarSelector(this, _T("Select Scalar to color Polydata by."), _T("Color by Scalar"),
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

