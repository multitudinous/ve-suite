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
#include <ves/conductor/vistab.h>
#include <ves/conductor/isosurfaces.h>
#include <ves/conductor/advancedisosurface.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/conductor/util/spinctld.h>

#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/choicdlg.h>
#include <wx/msgdlg.h>
#include <wx/button.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>

#include <iostream>
#include <vector>
#include <map>
using namespace ves::conductor;
using namespace ves::conductor::util;

///////////////////////////
BEGIN_EVENT_TABLE( Isosurfaces, wxDialog )
    ////@begin Isosurfaces event table entries
    EVT_RADIOBUTTON( ISOSURFACE_RBUTTON,          Isosurfaces::_onIsosurface )
    EVT_CHECKBOX( PRECOMPUTED_ISO_CHK,         Isosurfaces::_onPrecomputedIsosurface )
    EVT_SLIDER( ISOSURFACE_PLANE_SLIDER,     Isosurfaces::_onIsosurfacePlane )
    EVT_BUTTON( ADD_ISOSURFACE_BUTTON,       Isosurfaces::_onAddIsosurface )
    EVT_BUTTON( ADVANCED_ISOSURFACE_BUTTON,  Isosurfaces::_onAdvanced )
    EVT_COMMAND_SCROLL( ISOSURFACE_SPINCTRL,         Isosurfaces::_onSpinner )
    EVT_TEXT_ENTER( ISOSURFACE_SPINCTRL,        Isosurfaces::UpdateSlider )
    ////@end Isosurfaces event table entries
END_EVENT_TABLE()
Isosurfaces::Isosurfaces( )
{}
//////////////////////////////////////////////////////////
Isosurfaces::Isosurfaces( wxWindow* parent, wxWindowID id,
                          const wxString& caption,
                          const wxPoint& pos,
                          const wxSize& size, long style )
{

    advancediso = 0;

    Create( parent, id, caption, pos, size, style );
    wxSize displaySize = ::wxGetDisplaySize();
    int tempH = displaySize.GetHeight() - 480;
    wxRect dialogPosition( displaySize.GetWidth() - 427, displaySize.GetHeight() - tempH, 427, tempH );
    this->SetSize( dialogPosition );
}
///////////////////////////////////////////////////////////
Isosurfaces::~Isosurfaces()
{
    if( advancediso )
    {
        advancediso->Destroy();
        advancediso = 0;
    }
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
    _isoSpinner = 0;

    SetExtraStyle( GetExtraStyle() | wxWS_EX_BLOCK_EVENTS );
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit( this );
    GetSizer()->SetSizeHints( this );
    Centre();

    return true;
}
//////////////////////////////////
void Isosurfaces::CreateControls()
{
    Isosurfaces* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer( wxVERTICAL );
    itemDialog1->SetSizer( itemBoxSizer2 );

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox( itemDialog1, wxID_ANY, _T( "Isosurface Controls" ) );
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer( itemStaticBoxSizer3Static, wxVERTICAL );
    itemBoxSizer2->Add( itemStaticBoxSizer3, 0, wxGROW | wxALL, 5 );


    _useNearestPreComputedCheckBox = new wxCheckBox( itemDialog1, PRECOMPUTED_ISO_CHK, _T( "Use Nearest Precomputed Isosurface" ), wxDefaultPosition, wxDefaultSize, 0 );
    _useNearestPreComputedCheckBox->SetValue( false );
    itemStaticBoxSizer3->Add( _useNearestPreComputedCheckBox, 0, wxGROW | wxALL, 5 );

    wxStaticText* itemStaticText6 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Isosurface" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText6, 0, wxALIGN_LEFT | wxALL | wxADJUST_MINSIZE, 5 );

    wxBoxSizer* isoSizer = new wxBoxSizer( wxHORIZONTAL );

    _isoSpinner = new wxSpinCtrlDbl( *itemDialog1, ISOSURFACE_SPINCTRL, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 100, 0, 0.1, -1, wxEmptyString );

    _isoSurfaceSlider = new wxSlider( itemDialog1, ISOSURFACE_PLANE_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
//    itemStaticBoxSizer3->Add(_isoSurfaceSlider, 0, wxGROW|wxALL, 5);

    isoSizer->Add( _isoSpinner, 0, wxALIGN_LEFT | wxTOP | wxLEFT | wxRIGHT, 5 );
    isoSizer->Add( _isoSurfaceSlider, 1, wxGROW | wxALIGN_LEFT | wxLEFT | wxRIGHT | wxBOTTOM | wxEXPAND, 5 );
    itemStaticBoxSizer3->Add( isoSizer );

    wxBoxSizer* itemBoxSizer8 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer8, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );

    _computeButton = new wxButton( itemDialog1, ADD_ISOSURFACE_BUTTON, _T( "Compute Isosurface" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add( _computeButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    _advancedButton = new wxButton( itemDialog1, ADVANCED_ISOSURFACE_BUTTON, _T( "Advanced..." ), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add( _advancedButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T( "Close" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add( _closeButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
}
///////////////////////////////////////////////////////////
void Isosurfaces::SetActiveScalar( std::string activeScalar )
{
    _activeScalar = activeScalar;
}
////////////////////////////////////////////////////////////////
void Isosurfaces::SetAvailableScalars( wxArrayString scalarNames )
{
    _scalarNames.Clear();
    for( size_t i = 0; i < scalarNames.Count(); i++ )
    {
        _scalarNames.Add( scalarNames[i] );
    }
}
////////////////////////////////////////////////////////////////
void Isosurfaces::SetScalarRange( std::string activeScalar, std::vector<double> scalarRange )
{
    _scalarRange = scalarRange;

    if( _scalarRange.at( 1 ) == _scalarRange.at( 0 ) )
    {
        _isoSurfaceSlider->SetValue( 0 );
        _isoSpinner->SetValue( _scalarRange.at( 0 ) );
        _isoSurfaceSlider->Enable( false );
        _isoSpinner->Enable( false );
    }
    else
    {
        _isoSurfaceSlider->Enable( true );
        _isoSpinner->Enable( true );
    }

    if( tempScalarName.compare( _activeScalar ) )
    {
        tempScalarName = _activeScalar;
        _isoSpinner->SetRange( _scalarRange.at( 0 ), _scalarRange.at( 1 ) );
        _isoSpinner->SetValue( _scalarRange.at( 0 ) );
        _isoSurfaceSlider->SetValue( 0 );
    }

    _minValue = _scalarRange.at( 0 );
    _maxValue = _scalarRange.at( 1 );
}
////////////////////////////////
bool Isosurfaces::ShowToolTips()
{
    return true;
}
////////////////////////////////////////////////////////////////
wxBitmap Isosurfaces::GetBitmapResource( const wxString& name )
{
    wxUnusedVar( name );
    return wxNullBitmap;
}
////////////////////////////////////////////////////////////
wxIcon Isosurfaces::GetIconResource( const wxString& name )
{
    wxUnusedVar( name );
    return wxNullIcon;
}
/////////////////////////////////////////////////////////
void Isosurfaces::_onIsosurface( wxCommandEvent& WXUNUSED( event ) )
{}
////////////////////////////////////////////////////////////////////
void Isosurfaces::_onPrecomputedIsosurface( wxCommandEvent& WXUNUSED( event ) )
{}
/////////////////////////////////////////////////////////////
void Isosurfaces::_onIsosurfacePlane( wxCommandEvent& WXUNUSED( event ) )
{
    tempSliderScalar = _scalarRange.at( 0 ) + ( _scalarRange.at( 1 ) - _scalarRange.at( 0 ) ) / 100 * _isoSurfaceSlider->GetValue();
    _isoSpinner->SetValue( tempSliderScalar );
}
///////////////////////////////////////////////////////////
void Isosurfaces::_onAddIsosurface( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::Command* newCommand = new ves::open::xml::Command();
    newCommand->SetCommandName( "UPDATE_ISOSURFACE_SETTINGS" );

    ves::open::xml::DataValuePair* isosurfaceValue = new ves::open::xml::DataValuePair();
    isosurfaceValue->SetData( "Iso-Surface Value", static_cast<double>(( _isoSurfaceSlider->GetValue() ) ) );
    newCommand->AddDataValuePair( isosurfaceValue );

    ves::open::xml::DataValuePair* colorByScalar = new ves::open::xml::DataValuePair();
    colorByScalar->SetData( "Color By Scalar", _activeScalar );
    newCommand->AddDataValuePair( colorByScalar );

    ves::open::xml::DataValuePair* minValue = new ves::open::xml::DataValuePair();
    minValue->SetData( "Minimum Scalar Value", _minValue );
    newCommand->AddDataValuePair( minValue );

    ves::open::xml::DataValuePair* maxValue = new ves::open::xml::DataValuePair();
    maxValue->SetData( "Maximum Scalar Value", _maxValue );
    newCommand->AddDataValuePair( maxValue );

    ves::open::xml::DataValuePair* nearestPrecomputed = new ves::open::xml::DataValuePair();
    nearestPrecomputed->SetDataName( "Use Nearest Precomputed" );
    nearestPrecomputed->SetDataType( "UNSIGNED INT" );
    if( _useNearestPreComputedCheckBox->GetValue() )
    {
        nearestPrecomputed->SetDataValue( static_cast<unsigned int>( 1 ) );
    }
    else
    {
        nearestPrecomputed->SetDataValue( static_cast<unsigned int>( 0 ) );
    }
    newCommand->AddDataValuePair( nearestPrecomputed );

    try
    {
        dynamic_cast<Vistab*>( GetParent() )->SendUpdatedSettingsToXplorer( newCommand );
    }
    catch ( ... )
    {
        {
            wxMessageBox( _( "Invalid Parent" ),
                          _( "Communication Failure" ),
                          wxOK | wxICON_INFORMATION );
            if( newCommand )
            {
                delete newCommand;
                newCommand = 0;
            }
        }
    }
}
//////////////////////////////////////////////////////
void Isosurfaces::_onAdvanced( wxCommandEvent& WXUNUSED( event ) )
{
    int selectionIndex = 0;
    for( size_t i = 0; i < _scalarNames.Count(); i++ )
    {
        if( !_scalarNames[i].Cmp( wxString( _activeScalar.c_str(), wxConvUTF8 ) ) )
        {
            selectionIndex = i;
            break;
        }
    }

    if( !advancediso )
    {
        advancediso = new AdvancedIsosurface( this, SYMBOL_ADVANCEDISOSURFACES_IDNAME,
                                              SYMBOL_ADVANCEDISOSURFACES_TITLE,
                                              SYMBOL_ADVANCEDISOSURFACES_POSITION,
                                              SYMBOL_ADVANCEDISOSURFACES_SIZE,
                                              SYMBOL_ADVANCEDISOSURFACES_STYLE );

        advancediso->SetActiveScalar( _activeScalar );
        advancediso->SetScalarList( scalarlist );
        advancediso->PopulateList( _scalarNames );
    }

    advancediso->SetSize( GetRect() );
    advancediso->CentreOnParent();


    if( advancediso->ShowModal() == wxID_OK )
    {
        _minValue = advancediso->GetMinScalarValue();
        _maxValue = advancediso->GetMaxScalarValue();
        _activeScalar = advancediso->GetScalarName();
    }
}
//////////////////////////////////////////////////////
void Isosurfaces::_onSpinner( wxScrollEvent& WXUNUSED( event ) )
{
    tempSpinnerScalar = (( _isoSpinner->GetValue() - _scalarRange.at( 0 ) ) / ( _scalarRange.at( 1 ) - _scalarRange.at( 0 ) ) * 100 );
    _isoSurfaceSlider->SetValue( tempSpinnerScalar );
}
//////////////////////////////////////////////////////
void Isosurfaces::UpdateSlider( wxCommandEvent& event )
{
    double spinnerValue = 0;

    spinnerValue = (( _isoSpinner->GetValue() - _scalarRange.at( 0 ) ) / ( _scalarRange.at( 1 ) - _scalarRange.at( 0 ) ) * 100 );

    _isoSurfaceSlider->SetValue( spinnerValue );
}
//////////////////////////////////////////////////////
void Isosurfaces::InitializeScalarData( std::string activeScalar )
{
    if( tempScalarName.compare( _activeScalar ) )
    {
        tempScalarName = _activeScalar;
        _isoSpinner->SetValue( _scalarRange.at( 0 ) );
        _isoSurfaceSlider->SetValue( 0 );
    }
}
////////////////////////////////////////////////////////////
void Isosurfaces::SetScalarList( std::map<std::string, std::vector<double> > colorScalarRanges )
{
    scalarlist = colorScalarRanges;
}
