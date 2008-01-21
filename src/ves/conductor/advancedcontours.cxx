/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#include <ves/conductor/advancedcontours.h>

#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/radiobox.h>
#include <wx/checkbox.h>

#include <wx/bmpbuttn.h>
#include <wx/stattext.h>
#include <wx/statbox.h>

/*BEGIN_EVENT_TABLE( AdvancedContours, wxDialog )
////@begin AdvancedContours event table entries
   EVT_SLIDER        ( OPACITY_SLIDER,          AdvancedContours::_onContourOpacity )
   EVT_RADIOBOX      (CONTOUR_TYPE_RBOX,        AdvancedContours::_onContourType)
   EVT_SLIDER        ( WARPED_SCALE_SLIDER,     AdvancedContours::_onWarpedContour )
   EVT_SLIDER        ( LOD_SLIDER,              AdvancedContours::_onContourLOD )
////@end AdvancedContours event table entries
END_EVENT_TABLE()*/
using namespace ves::conductor;



AdvancedContours::AdvancedContours( wxWindow* parent, wxWindowID id,
                                    const wxString& caption, const wxPoint& pos,
                                    const wxSize& size, long style )
{
    Create( parent, id, caption, pos, size, style );
    wxSize displaySize = ::wxGetDisplaySize();
    int tempH = displaySize.GetHeight() - 480;
    wxRect dialogPosition( displaySize.GetWidth() - 427, 200, 427, tempH );
    this->SetSize( dialogPosition );
}

bool AdvancedContours::Create( wxWindow* parent, wxWindowID id,
                               const wxString& caption, const wxPoint& pos,
                               const wxSize& size, long style )
{
////@begin AdvancedContours member initialisation
    _opacitySlider = 0;
    _warpedScaleSlider = 0;
    _LODSlider = 0;
    _contourTypeRBox = 0;
    _warpOptionCBox = 0;
    /*_opacity = 1.0;
    _warpedScale = .5;
    _LOD = 0.0;
    _planeType = "Graduated";*/
////@end AdvancedContours member initialisation

////@begin AdvancedContours creation
    SetExtraStyle( GetExtraStyle() | wxWS_EX_BLOCK_EVENTS );
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit( this );
    GetSizer()->SetSizeHints( this );
    Centre();
////@end AdvancedContours creation
    return true;
}

/*!
 * Control creation for AdvancedContours
 */

void AdvancedContours::CreateControls()
{
////@begin AdvancedContours content construction
    // Generated by DialogBlocks, Fri 21 Apr 2006 10:36:58 CDT

    AdvancedContours* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer( wxVERTICAL );
    itemDialog1->SetSizer( itemBoxSizer2 );

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox( itemDialog1, wxID_ANY, _T( "Advanced Contour Controls" ) );
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer( itemStaticBoxSizer3Static, wxVERTICAL );
    itemBoxSizer2->Add( itemStaticBoxSizer3, 0, wxGROW | wxALL, 5 );

    wxStaticText* itemStaticText4 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Contour Opacity" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText4, 0, wxALIGN_LEFT | wxLEFT | wxRIGHT | wxTOP | wxADJUST_MINSIZE, 5 );

    _opacitySlider = new wxSlider( itemDialog1, OPACITY_SLIDER, 100, 0, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _opacitySlider, 0, wxGROW | wxLEFT | wxRIGHT, 5 );

    wxBoxSizer* itemBoxSizer6 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer6, 0, wxGROW | wxALL, 5 );

    wxStaticText* itemStaticText7 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Transparent" ), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer6->Add( itemStaticText7, 0, wxALIGN_CENTER_VERTICAL | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText8 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Opaque" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer6->Add( itemStaticText8, 1, wxALIGN_CENTER_VERTICAL | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText9 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Warped Contour Scale" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText9, 0, wxALIGN_LEFT | wxLEFT | wxRIGHT | wxTOP | wxADJUST_MINSIZE, 5 );

    _warpedScaleSlider = new wxSlider( itemDialog1, WARPED_SCALE_SLIDER, 50, 0, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _warpedScaleSlider, 0, wxGROW | wxLEFT | wxRIGHT, 5 );

    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer11, 0, wxGROW | wxALL, 5 );

    wxStaticText* itemStaticText12 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Lower" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer11->Add( itemStaticText12, 0, wxALIGN_CENTER_VERTICAL | wxLEFT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Higher" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer11->Add( itemStaticText13, 1, wxALIGN_CENTER_VERTICAL | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText14 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Contour LOD" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText14, 0, wxALIGN_LEFT | wxLEFT | wxRIGHT | wxTOP | wxADJUST_MINSIZE, 5 );

    _LODSlider = new wxSlider( itemDialog1, LOD_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _LODSlider, 0, wxGROW | wxLEFT | wxRIGHT, 5 );

    wxBoxSizer* itemBoxSizer16 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer16, 0, wxGROW | wxALL, 5 );

    wxStaticText* itemStaticText17 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Higher Detail" ), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer16->Add( itemStaticText17, 0, wxALIGN_CENTER_VERTICAL | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText18 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Lower Detail" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer16->Add( itemStaticText18, 1, wxALIGN_CENTER_VERTICAL | wxLEFT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxString itemRadioBox6Strings[] = {
                                          _T( "Graduated" ),
                                          _T( "Banded" ),
                                          _T( "Lined" )
                                      };

    wxBoxSizer* _misc = new wxBoxSizer( wxHORIZONTAL );

    _contourTypeRBox = new wxRadioBox( itemDialog1, CONTOUR_TYPE_RBOX, _T( "Contour Type" ), wxDefaultPosition, wxDefaultSize, 3, itemRadioBox6Strings, 1, wxRA_SPECIFY_ROWS );
    _warpOptionCBox = new wxCheckBox( itemDialog1, WARP_OPTION_CHK, _T( "Warp Option" ), wxDefaultPosition, wxDefaultSize, 0 );
    _warpOptionCBox->SetValue( false );

    _misc->Add( _contourTypeRBox, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
    _misc->Add( _warpOptionCBox, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    itemStaticBoxSizer3->Add( _misc, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );

    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T( "Close" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( _closeButton, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );
}
/////////////////////////////////////////////////
void AdvancedContours::SetOpacity( double opacity )
{
    if( _opacitySlider )
    {
        _opacitySlider->SetValue( static_cast<int>( 100*opacity ) );
    }
}
///////////////////////////////////////////////////////
void AdvancedContours::SetWarpedScale( double warpScale )
{
    if( _warpedScaleSlider )
    {
        _warpedScaleSlider->SetValue( static_cast<int>( 100*warpScale ) );
    }
}
/////////////////////////////////////////
void AdvancedContours::SetLOD( double LOD )
{
    if( _LODSlider )
    {
        _LODSlider->SetValue( static_cast<int>( 100*LOD ) );
    }
}
/////////////////////////////////////
bool AdvancedContours::ShowToolTips()
{
    return true;
}

/*!
 * Get bitmap resources
 */

wxBitmap AdvancedContours::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin AdvancedContours bitmap retrieval
    wxUnusedVar( name );
    return wxNullBitmap;
////@end AdvancedContours bitmap retrieval
}

/*!
 * Get icon resources
 */

wxIcon AdvancedContours::GetIconResource( const wxString& name )
{
    // Icon retrieval
////@begin AdvancedContours icon retrieval
    wxUnusedVar( name );
    return wxNullIcon;
////@end AdvancedContours icon retrieval
}
/////////////////////////////////////
double AdvancedContours::GetOpacity()
{
    return  static_cast<double>( _opacitySlider->GetValue() / 100.0 );
}
/////////////////////////////////////////
double AdvancedContours::GetWarpedScale()
{
    return static_cast<double>( _warpedScaleSlider->GetValue() / 100.0 );
}
/////////////////////////////////
double AdvancedContours::GetLOD()
{
    return static_cast<double>( _LODSlider->GetValue() / 100.0 );
}
///////////////////////////////////////////////////////
void AdvancedContours::SetContourType( std::string type )
{
    _contourTypeRBox->SetStringSelection( wxString( type.c_str(), wxConvUTF8 ) );
}
/////////////////////////////////////////////////////
void AdvancedContours::SetWarpOption( bool warpOption )
{
    _warpOptionCBox->SetValue( warpOption );
}
//////////////////////////////////////
bool AdvancedContours::GetWarpOption()
{
    return _warpOptionCBox->GetValue();
}
//////////////////////////////////////////////
std::string AdvancedContours::GetContourType()
{
    return ConvertUnicode( _contourTypeRBox->GetStringSelection().c_str() );
}

