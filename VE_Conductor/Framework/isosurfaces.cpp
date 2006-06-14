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
///////////////////////////
BEGIN_EVENT_TABLE( Isosurfaces, wxDialog )
////@begin Isosurfaces event table entries
   EVT_RADIOBUTTON      (ISOSURFACE_RBUTTON,          Isosurfaces::_onIsosurface)
   EVT_CHECKBOX         (PRECOMPUTED_ISO_CHK,         Isosurfaces::_onPrecomputedIsosurface)
   EVT_SLIDER           (ISOSURFACE_PLANE_SLIDER,     Isosurfaces::_onIsosurfacePlane)
   EVT_BUTTON           (ADD_ISOSURFACE_BUTTON,       Isosurfaces::_onAddIsosurface)
   EVT_BUTTON           (ADVANCED_ISOSURFACE_BUTTON,  Isosurfaces::_onAdvanced)
////@end Isosurfaces event table entries
END_EVENT_TABLE()
Isosurfaces::Isosurfaces( )
{

}
//////////////////////////////////////////////////////////
Isosurfaces::Isosurfaces( wxWindow* parent, wxWindowID id,
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
bool Isosurfaces::Create( wxWindow* parent, wxWindowID id, 
                       const wxString& caption, 
                       const wxPoint& pos, 
                       const wxSize& size, long style )
{
   _useNearestPreComputedCheckBox = 0;
   _isoSurfaceSlider = 0;
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
void Isosurfaces::CreateControls()
{    
    Isosurfaces* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Isosurface Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

   
    _useNearestPreComputedCheckBox = new wxCheckBox( itemDialog1, PRECOMPUTED_ISO_CHK, _T("Use Nearest Precomputed Isosurface"), wxDefaultPosition, wxDefaultSize, 0 );
    _useNearestPreComputedCheckBox->SetValue(false);
    itemStaticBoxSizer3->Add(_useNearestPreComputedCheckBox, 0, wxGROW|wxALL, 5);

    wxStaticText* itemStaticText6 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Isosurface"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer2->Add(itemStaticText6, 0, wxALIGN_LEFT|wxALL|wxADJUST_MINSIZE, 5);

    _isoSurfaceSlider = new wxSlider( itemDialog1, ISOSURFACE_PLANE_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemBoxSizer2->Add(_isoSurfaceSlider, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer8 = new wxBoxSizer(wxHORIZONTAL);
    itemBoxSizer2->Add(itemBoxSizer8, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    _computeButton = new wxButton( itemDialog1, ADD_ISOSURFACE_BUTTON, _T("Compute Isosurface"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add(_computeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    _advancedButton = new wxButton( itemDialog1, ADVANCED_ISOSURFACE_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add(_advancedButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
}
////////////////////////////////////////////////////////
void Isosurfaces::SetActiveScalar(std::string activeScalar)
{
   _activeScalar = activeScalar;
   _colorByScalarName = _activeScalar;
}
////////////////////////////////////////////////////////////////
void Isosurfaces::SetAvailableScalars(wxArrayString scalarNames)
{
   for(size_t i = 0; i < scalarNames.Count(); i++)
   {
      _scalarNames.Add(scalarNames[i]);
   }
}
////////////////////////////////
bool Isosurfaces::ShowToolTips()
{
    return true;
}
////////////////////////////////////////////////////////////////
wxBitmap Isosurfaces::GetBitmapResource( const wxString& name )
{
   wxUnusedVar(name);
   return wxNullBitmap;
}
////////////////////////////////////////////////////////////
wxIcon Isosurfaces::GetIconResource( const wxString& name )
{
   wxUnusedVar(name);
   return wxNullIcon;
}
/////////////////////////////////////////////////////////
void Isosurfaces::_onIsosurface( wxCommandEvent& WXUNUSED(event) )
{

}
////////////////////////////////////////////////////////////////////
void Isosurfaces::_onPrecomputedIsosurface( wxCommandEvent& WXUNUSED(event) )
{

}
/////////////////////////////////////////////////////////////
void Isosurfaces::_onIsosurfacePlane( wxCommandEvent& WXUNUSED(event) )
{
}
///////////////////////////////////////////////////////////
void Isosurfaces::_onAddIsosurface( wxCommandEvent& WXUNUSED(event) )
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
   wxSingleChoiceDialog scalarSelector(this, _T("Select Scalar to color isosurface by."), _T("Color by Scalar"),
                                   _scalarNames);
   int displayWidth, displayHeight = 0;
   ::wxDisplaySize(&displayWidth,&displayHeight);
  
   wxRect bbox = GetRect();

   int width,height = 0;
   GetSize(&width,&height);
   scalarSelector.SetSize(wxRect( 2*displayWidth/3, bbox.GetBottomRight().y, 
                        width, height));
   //scalarSelector.SetStringSelection(_colorByScalarName);
   if (scalarSelector.ShowModal() == wxID_OK)
   {
      _colorByScalarName = scalarSelector.GetStringSelection();
   }
}

