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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/vectors.h"
#include "VE_Conductor/Framework/advancedvectors.h"
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/statbmp.h>
#include <wx/button.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <iostream>

////@begin XPM images
////@end XPM images

/*!
 * Application instance implementation
 */
/*
////@begin implement app
IMPLEMENT_APP( VectorsApp )
////@end implement app
*/
/*!
 * VectorsApp type definition
 */
/*
IMPLEMENT_CLASS( VectorsApp, wxApp )
*/
/*!
 * VectorsApp event table definition
 */
/*
BEGIN_EVENT_TABLE( VectorsApp, wxApp )

////@begin VectorsApp event table entries
////@end VectorsApp event table entries

END_EVENT_TABLE()
*/
/*!
 * Constructor for VectorsApp
 */
/*
VectorsApp::VectorsApp()
{
////@begin VectorsApp member initialisation
////@end VectorsApp member initialisation
}
*/
/*!
 * Initialisation for VectorsApp
 */
/*
bool VectorsApp::OnInit()
{    
////@begin VectorsApp initialisation
    // Remove the comment markers above and below this block
    // to make permanent changes to the code.

#if wxUSE_XPM
    wxImage::AddHandler(new wxXPMHandler);
#endif
#if wxUSE_LIBPNG
    wxImage::AddHandler(new wxPNGHandler);
#endif
#if wxUSE_LIBJPEG
    wxImage::AddHandler(new wxJPEGHandler);
#endif
#if wxUSE_GIF
    wxImage::AddHandler(new wxGIFHandler);
#endif
////@end VectorsApp initialisation

    return true;
}
*/
/*!
 * Cleanup for VectorsApp
 */
/*
int VectorsApp::OnExit()
{    
////@begin VectorsApp cleanup
    return wxApp::OnExit();
////@end VectorsApp cleanup
}
*/

/*!
 * Vectors type definition
 */

//IMPLEMENT_DYNAMIC_CLASS( Vectors, wxDialog )

/*!
 * Vectors event table definition
 */

BEGIN_EVENT_TABLE( Vectors, wxDialog )
////@begin Vectors event table entrieS
   EVT_RADIOBOX      (VECTOR_DIR_RBOX,            Vectors::_onDirection)
   EVT_RADIOBUTTON   (MULTIPLE_PREVECTOR_RBUTTON, Vectors::_onMultiplePlanes)
   EVT_CHECKBOX      (MULTIPLE_PREVECTOR_CHK,     Vectors::_onCyclePlanes)
   EVT_RADIOBUTTON   (SINGLE_PREVECTOR_RBUTTON,   Vectors::_onSinglePlane)
   EVT_CHECKBOX      (SINGLE_PREVECTOR_CHK,       Vectors::_onPrecomputedPlane)
   EVT_SLIDER        (VECTOR_PLANE_SLIDER,        Vectors::_onPlane)
   EVT_BUTTON        (ADD_VECTOR_PLANE_BUTTON,    Vectors::_onAddPlane)
   EVT_BUTTON        (ADVANCED_VECTOR_BUTTON,     Vectors::_onAdvanced)
////@end Vectors event table entries
END_EVENT_TABLE()

/*!
 * Vectors constructors
 */
/*
Vectors::Vectors(VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn)
:wxDialog(NULL,-1, wxString("Vectors"), 
		wxDefaultPosition, wxDefaultSize,
      (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP)
{
   wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
   this->SetSize( dialogPosition );

   xplorerPtr = VjObs::_duplicate( veEngine );
   domManager = domManagerIn;

   CreateControls();
}
*/
Vectors::Vectors( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
    Create(parent, id, caption, pos, size, style);
    wxSize displaySize = ::wxGetDisplaySize();
    wxRect dialogPosition( displaySize.GetWidth()-427, 440, 427, displaySize.GetHeight()-480 );
    this->SetSize( dialogPosition );
}

/*!
 * Vectors creator
 */

bool Vectors::Create( wxWindow* parent, wxWindowID id,
                  const wxString& caption, const wxPoint& pos, 
                  const wxSize& size, long style )
{
////@begin Vectors member initialisation
   itemRadioBox5 = 0;
   itemRadioButton8 = 0;
   itemCheckBox9 = 0;
   itemRadioButton11 = 0;
   itemCheckBox12 = 0;
   itemSlider14 = 0;
   itemButton16 = 0;
   itemButton17 = 0;
////@end Vectors member initialisation

////@begin Vectors creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
////@end Vectors creation
    return true;
}

void Vectors::CreateControls()
{    
////@begin Vectors content construction
    // Generated by DialogBlocks, Fri 21 Apr 2006 10:34:25 CDT (unregistered)

   Vectors* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Vector Controls"));
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer(itemStaticBoxSizer3Static, wxVERTICAL);
    itemBoxSizer2->Add(itemStaticBoxSizer3, 0, wxGROW|wxALL, 5);

    wxBoxSizer* itemBoxSizer4 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer4, 0, wxGROW|wxALL, 5);

    wxString itemRadioBox5Strings[] = {
        _T("x"),
        _T("y"),
        _T("z"),
        _T("By Wand")
    };
    wxRadioBox* itemRadioBox5 = new wxRadioBox( itemDialog1, ID_V_RADIOBOX, _T("Direction"), wxDefaultPosition, wxDefaultSize, 4, itemRadioBox5Strings, 1, wxRA_SPECIFY_COLS );
    itemBoxSizer4->Add(itemRadioBox5, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
/*
    wxBitmap itemStaticBitmap6Bitmap(itemDialog1->GetBitmapResource(wxT("../../../../../../home/users/jaredabo/TSVEG/VE_Suite/VE_TestSuite/vector2.png")));
    wxStaticBitmap* itemStaticBitmap6 = new wxStaticBitmap( itemDialog1, wxID_STATIC, itemStaticBitmap6Bitmap, wxDefaultPosition, wxSize(90, 93), 0 );
    itemBoxSizer4->Add(itemStaticBitmap6, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
*/
    wxStaticBox* itemStaticBoxSizer7Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Multiple Planes"));
    wxStaticBoxSizer* itemStaticBoxSizer7 = new wxStaticBoxSizer(itemStaticBoxSizer7Static, wxVERTICAL);
    itemStaticBoxSizer3->Add(itemStaticBoxSizer7, 0, wxGROW|wxALL, 5);

    wxRadioButton* itemRadioButton8 = new wxRadioButton( itemDialog1, ID_V_RADIOBUTTON, _T("All Precomputed Surfaces"), wxDefaultPosition, wxDefaultSize, 0 );
    itemRadioButton8->SetValue(false);
    itemStaticBoxSizer7->Add(itemRadioButton8, 0, wxALIGN_LEFT|wxALL, 5);

    wxCheckBox* itemCheckBox9 = new wxCheckBox( itemDialog1, ID_V_CHECKBOX, _T("Cycle Precomputed Surfaces"), wxDefaultPosition, wxDefaultSize, 0 );
    itemCheckBox9->SetValue(false);
    itemStaticBoxSizer7->Add(itemCheckBox9, 0, wxALIGN_LEFT|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer10Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Single Plane"));
    wxStaticBoxSizer* itemStaticBoxSizer10 = new wxStaticBoxSizer(itemStaticBoxSizer10Static, wxVERTICAL);
    itemStaticBoxSizer3->Add(itemStaticBoxSizer10, 0, wxGROW|wxALL, 5);

    wxRadioButton* itemRadioButton11 = new wxRadioButton( itemDialog1, ID_V_RADIOBUTTON1, _T("Specify a Single Plane"), wxDefaultPosition, wxDefaultSize, 0 );
    itemRadioButton11->SetValue(false);
    itemStaticBoxSizer10->Add(itemRadioButton11, 0, wxALIGN_LEFT|wxALL, 5);

    wxCheckBox* itemCheckBox12 = new wxCheckBox( itemDialog1, ID_V_CHECKBOX1, _T("Use Nearest Precomputed Plane"), wxDefaultPosition, wxDefaultSize, 0 );
    itemCheckBox12->SetValue(false);
    itemStaticBoxSizer10->Add(itemCheckBox12, 0, wxALIGN_LEFT|wxALL, 5);

    wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _T("Plane"), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add(itemStaticText13, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE, 5);

    wxSlider* itemSlider14 = new wxSlider( itemDialog1, ID_V_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize(300, -1), wxSL_HORIZONTAL|wxSL_LABELS );
    itemStaticBoxSizer3->Add(itemSlider14, 0, wxGROW|wxLEFT|wxRIGHT|wxBOTTOM, 5);

    wxBoxSizer* itemBoxSizer15 = new wxBoxSizer(wxHORIZONTAL);
    itemStaticBoxSizer3->Add(itemBoxSizer15, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxButton* itemButton16 = new wxButton( itemDialog1, wxID_OK, _T("Add Plane"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer15->Add(itemButton16, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* itemButton17 = new wxButton( itemDialog1, ADVANCED_VECTOR_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer15->Add(itemButton17, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer15->Add(_closeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

////@end Vectors content construction
}

/*!
 * Should we show tooltips?
 */

bool Vectors::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */
/*
wxBitmap Vectors::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin Vectors bitmap retrieval
    wxUnusedVar(name);
    if (name == _T("../../../../../../home/users/jaredabo/TSVEG/VE_Suite/VE_TestSuite/vector2.png"))
    {
        wxBitmap bitmap(_T("../../../../../../home/users/jaredabo/TSVEG/VE_Suite/VE_TestSuite/vector2.png"), wxBITMAP_TYPE_PNG);
        return bitmap;
    }
    return wxNullBitmap;
////@end Vectors bitmap retrieval
}
*/
/*!
 * Get icon resources
 */

wxIcon Vectors::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin Vectors icon retrieval
    wxUnusedVar(name);
    return wxNullIcon;
////@end Vectors icon retrieval
}
//////////////////////////////////////////////////////////
void Vectors::_onAdvanced( wxCommandEvent& WXUNUSED(event) )
{
//   adVector = new AdvancedVectors(xplorerPtr, domManager);
   adVector = new AdvancedVectors(this,                
                  SYMBOL_ADVANCEDVECTORS_IDNAME, 
                  SYMBOL_ADVANCEDVECTORS_TITLE,
                  SYMBOL_ADVANCEDVECTORS_POSITION,
                  SYMBOL_ADVANCEDVECTORS_SIZE, 
                  SYMBOL_ADVANCEDVECTORS_STYLE );
   adVector->ShowModal();
std::cout<<"ADVANCEDVECTORS WORKING"<<std::endl;
}
/*!
 * wxEVT_COMMAND_RADIOBOX_SELECTED event handler for ID_RADIOBOX
 */

void Vectors::_onDirection( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_RADIOBOX_SELECTED event handler for ID_RADIOBOX in Contours.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_RADIOBOX_SELECTED event handler for ID_RADIOBOX in Contours. 
}

/*!
 * wxEVT_COMMAND_RADIOBUTTON_SELECTED event handler for ID_RADIOBUTTON
 */

void Vectors::_onMultiplePlanes( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_RADIOBUTTON_SELECTED event handler for ID_RADIOBUTTON in Contours.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_RADIOBUTTON_SELECTED event handler for ID_RADIOBUTTON in Contours. 
}

/*!
 * wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX
 */

void Vectors::_onCyclePlanes( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX in Contours.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX in Contours. 
}

/*!
 * wxEVT_COMMAND_RADIOBUTTON_SELECTED event handler for ID_RADIOBUTTON1
 */

void Vectors::_onSinglePlane( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_RADIOBUTTON_SELECTED event handler for ID_RADIOBUTTON1 in Contours.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_RADIOBUTTON_SELECTED event handler for ID_RADIOBUTTON1 in Contours. 
}

/*!
 * wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX1
 */

void Vectors::_onPrecomputedPlane( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX1 in Contours.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_CHECKBOX_CLICKED event handler for ID_CHECKBOX1 in Contours. 
}

/*!
 * wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER
 */

void Vectors::_onPlane( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER in Contours.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_SLIDER_UPDATED event handler for ID_SLIDER in Contours. 
}

/*!
 * wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1
 */

void Vectors::_onAddPlane( wxCommandEvent& event )
{
////@begin wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1 in Contours.
    // Before editing this code, remove the block markers.
    event.Skip();
////@end wxEVT_COMMAND_BUTTON_CLICKED event handler for ID_BUTTON1 in Contours. 
}
