/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include <ves/conductor/util/UI_TransientDialog.h>
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/advancedstreamlines.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/checkbox.h>
#include <wx/icon.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/bmpbuttn.h>

using namespace ves::conductor;

BEGIN_EVENT_TABLE( AdvancedStreamlines, wxDialog )
    EVT_SLIDER( LINE_DIAMETER_SLIDER, AdvancedStreamlines::_OnLineDiameter )
    EVT_SLIDER( GLOW_SLIDER, AdvancedStreamlines::_OnGlowStrength )
    EVT_BUTTON( PARTICLE_TRACKING, AdvancedStreamlines::_OnParticleTracking )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AdvancedStreamlines::AdvancedStreamlines( wxWindow* parent,
                                          wxWindowID id,
                                          const wxString& caption,
                                          const wxPoint& pos,
                                          const wxSize& size,
                                          long style )
{
    Create( parent, id, caption, pos, size, style );
    wxSize displaySize = ::wxGetDisplaySize();
    wxRect dialogPosition( displaySize.GetWidth() - 427, 440, 427, displaySize.GetHeight() - 480 );
    SetSize( dialogPosition );

    _parentLocal = parent;
}
////////////////////////////////////////////////////////////////////////////////
bool AdvancedStreamlines::Create( wxWindow* parent,
                                  wxWindowID id,
                                  const wxString& caption,
                                  const wxPoint& pos,
                                  const wxSize& size,
                                  long style )
{
    _propagationSlider = 0;
    _integrationSlider = 0;
    _sphereArrowParticleSlider = 0;
    _diameterSlider = 0;
    _glowSlider = 0;
    _lastSeedPtCheck = 0;
    _streamArrowCheck = 0;

    particleControls = 0;

    SetExtraStyle( GetExtraStyle() | wxWS_EX_BLOCK_EVENTS );
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit( this );
    GetSizer()->SetSizeHints( this );
    Centre();

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::CreateControls()
{
    AdvancedStreamlines* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer( wxVERTICAL );
    itemDialog1->SetSizer( itemBoxSizer2 );

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox( itemDialog1, wxID_ANY, _T( "Advanced Streamline Controls" ) );
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer( itemStaticBoxSizer3Static, wxVERTICAL );
    itemBoxSizer2->Add( itemStaticBoxSizer3, 0, wxGROW | wxALL, 5 );

    wxStaticText* itemStaticText4 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Propagation (Total) Time" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText4, 0, wxALIGN_LEFT | wxLEFT | wxRIGHT | wxTOP | wxADJUST_MINSIZE, 5 );

    _propagationSlider = new wxSlider( itemDialog1, PROPOGATION_SLIDER, 100, 1, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _propagationSlider, 0, wxGROW | wxLEFT | wxRIGHT, 5 );

    wxBoxSizer* itemBoxSizer6 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer6, 0, wxGROW | wxLEFT | wxRIGHT | wxBOTTOM, 5 );

    wxStaticText* itemStaticText7 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Shorter" ), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer6->Add( itemStaticText7, 0, wxALIGN_TOP | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText8 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Longer" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer6->Add( itemStaticText8, 1, wxALIGN_TOP | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText9 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Integration Step" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText9, 0, wxALIGN_LEFT | wxLEFT | wxRIGHT | wxTOP | wxADJUST_MINSIZE, 5 );

    _integrationSlider = new wxSlider( itemDialog1, INTEGRATION_STEP_SLIDER, 1000, 1, 5000, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _integrationSlider, 0, wxGROW | wxLEFT | wxRIGHT, 5 );

    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer11, 0, wxGROW | wxLEFT | wxRIGHT | wxBOTTOM, 5 );

    wxStaticText* itemStaticText12 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Smaller" ), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT | wxSTATIC_BORDER );
    itemBoxSizer11->Add( itemStaticText12, 0, wxALIGN_TOP | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText13 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Larger" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer11->Add( itemStaticText13, 1, wxALIGN_TOP | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText19 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Sphere/Arrow/Particle Size(%)" ), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemStaticBoxSizer3->Add( itemStaticText19, 0, wxALIGN_LEFT | wxLEFT | wxRIGHT | wxTOP | wxADJUST_MINSIZE, 5 );

    _sphereArrowParticleSlider = new wxSlider( itemDialog1, SPHERE_SIZE_SLIDER, 5, 1, 50, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _sphereArrowParticleSlider, 0, wxGROW | wxLEFT | wxRIGHT | wxBOTTOM, 5 );

    wxStaticText* itemStaticText21 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Line Diameter" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText21, 0, wxALIGN_LEFT | wxLEFT | wxRIGHT | wxTOP | wxADJUST_MINSIZE, 5 );

    _diameterSlider = new wxSlider( itemDialog1, LINE_DIAMETER_SLIDER, 80, 0, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _diameterSlider, 0, wxGROW | wxLEFT | wxRIGHT, 5 );

    wxBoxSizer* itemBoxSizer23 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer23, 0, wxGROW | wxLEFT | wxRIGHT | wxBOTTOM, 5 );

    wxStaticText* itemStaticText24 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Decrease Size" ), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer23->Add( itemStaticText24, 0, wxALIGN_TOP | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText25 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Increase Size" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer23->Add( itemStaticText25, 1, wxALIGN_TOP | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );


    wxStaticText* itemStaticText31 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Glow Strength" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText31, 0, wxALIGN_LEFT | wxLEFT | wxRIGHT | wxTOP | wxADJUST_MINSIZE, 5 );

    _glowSlider = new wxSlider( itemDialog1, GLOW_SLIDER, 4, 0, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _glowSlider, 0, wxGROW | wxLEFT | wxRIGHT, 5 );

    wxBoxSizer* itemBoxSizer33 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer33, 0, wxGROW | wxLEFT | wxRIGHT | wxBOTTOM, 5 );

    wxStaticText* itemStaticText34 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Weaker" ), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    itemBoxSizer33->Add( itemStaticText34, 0, wxALIGN_TOP | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );

    wxStaticText* itemStaticText35 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Stronger" ), wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
    itemBoxSizer33->Add( itemStaticText35, 1, wxALIGN_TOP | wxLEFT | wxRIGHT | wxBOTTOM | wxADJUST_MINSIZE, 5 );


    wxBoxSizer* itemBoxSizer26 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer26, 0, wxGROW | wxALL, 5 );

    _lastSeedPtCheck = new wxCheckBox( itemDialog1, USE_SEED_POINT_CHK, _T( "Use Last Seed Point" ), wxDefaultPosition, wxDefaultSize, 0 );
    _lastSeedPtCheck->SetValue( false );
    itemBoxSizer26->Add( _lastSeedPtCheck, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    _streamArrowCheck = new wxCheckBox( itemDialog1, ARROWS_CHK, _T( "Stream Arrows" ), wxDefaultPosition, wxDefaultSize, 0 );
    _streamArrowCheck->SetValue( false );
    itemBoxSizer26->Add( _streamArrowCheck, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    wxBoxSizer* _buttons = new wxBoxSizer( wxHORIZONTAL );

    wxButton* _particleButton = new wxButton( itemDialog1, PARTICLE_TRACKING, _T( "Particle" ), wxDefaultPosition, wxDefaultSize, 0 );
    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T( "Close" ), wxDefaultPosition, wxDefaultSize, 0 );
    _buttons->Add( _particleButton, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );
    _buttons->Add( _closeButton, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );

    itemStaticBoxSizer3->Add( _buttons, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );
}
////////////////////////////////////////////////////////////////////////////////
bool AdvancedStreamlines::ShowToolTips()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
wxBitmap AdvancedStreamlines::GetBitmapResource( const wxString& name )
{
    wxUnusedVar( name );
    return wxNullBitmap;
}
////////////////////////////////////////////////////////////////////////////////
wxIcon AdvancedStreamlines::GetIconResource( const wxString& name )
{
    wxUnusedVar( name );
    return wxNullIcon;
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::SetPropagationSize( double value )
{
    if( _propagationSlider )
    {
        _propagationSlider->SetValue( static_cast< int >( value ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::SetIntegrationStepSize( double value )
{
    if( _integrationSlider )
    {
        _integrationSlider->SetValue( static_cast< int >( value ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::SetSphereArrowParticleSize( double value )
{
    if( _sphereArrowParticleSlider )
    {
        _sphereArrowParticleSlider->SetValue( static_cast< int >( value ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::SetLineDiameter( double value )
{
    if( _diameterSlider )
    {
        _diameterSlider->SetValue( static_cast< int >( value ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::SetGlowStrength( double value )
{
    if( _glowSlider )
    {
        _glowSlider->SetValue( value );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::SetStreamArrow( bool value )
{
    if( _streamArrowCheck )
    {
        _streamArrowCheck->SetValue( value );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::SetUseLastSeedPt( bool value )
{
    if( _lastSeedPtCheck )
    {
        _lastSeedPtCheck->SetValue( value );
    }
}
////////////////////////////////////////////////////////////////////////////////
double AdvancedStreamlines::GetPropagationSize()
{
    return  static_cast< double >( _propagationSlider->GetValue() );
}
////////////////////////////////////////////////////////////////////////////////
double AdvancedStreamlines::GetIntegrationStepSize()
{
    return static_cast< double >( _integrationSlider->GetValue() );
}
////////////////////////////////////////////////////////////////////////////////
double AdvancedStreamlines::GetSphereArrowParticleSize()
{
    return static_cast< double >( _sphereArrowParticleSlider->GetValue() );
}
////////////////////////////////////////////////////////////////////////////////
double AdvancedStreamlines::GetLineDiameter()
{
    return static_cast< double >( _diameterSlider->GetValue() );
}
////////////////////////////////////////////////////////////////////////////////
double AdvancedStreamlines::GetGlowStrength()
{
    return static_cast< double >( _glowSlider->GetValue() );
}
////////////////////////////////////////////////////////////////////////////////
bool AdvancedStreamlines::GetUseLastSeedPoint()
{
    return _lastSeedPtCheck->GetValue();
}
////////////////////////////////////////////////////////////////////////////////
bool AdvancedStreamlines::GetStreamArrow()
{
    return _streamArrowCheck->GetValue();
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::_OnLineDiameter( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr dvp = new ves::open::xml::DataValuePair();
    ves::open::xml::CommandSharedPtr command = new ves::open::xml::Command();

    double value = static_cast< double >( _diameterSlider->GetValue() );

    dvp->SetData( std::string( "Size" ), value );

    command->SetCommandName( std::string( "LIVE_STREAMLINE_UPDATE" ) );
    command->AddDataValuePair( dvp );

    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::_OnGlowStrength( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr dvp = new ves::open::xml::DataValuePair();
    ves::open::xml::CommandSharedPtr command = new ves::open::xml::Command();

    double value = static_cast< double >( _glowSlider->GetValue() );

    dvp->SetData( std::string( "Glow" ), value );

    command->SetCommandName( std::string( "LIVE_STREAMLINE_UPDATE" ) );
    command->AddDataValuePair( dvp );

    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AdvancedStreamlines::_OnParticleTracking( wxCommandEvent& WXUNUSED( event ) )
{
    if( particleControls )
    {
        delete particleControls;
        particleControls = 0;
    }

    particleControls = new ves::conductor::util::UI_TransientDialog( 19, this, PARTICLE_TRACKING_DIALOG );
    particleControls->SetTitle( wxString( "Particle Controls", wxConvUTF8 ) );

    particleControls->ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
