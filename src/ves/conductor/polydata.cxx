/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/vistab.h>
#include <ves/conductor/polydata.h>
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/PolyDataScalarControlDialog.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

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
#include <iostream>
using namespace ves::conductor;

///////////////////////////
BEGIN_EVENT_TABLE( Polydata, wxDialog )
    ////@begin polydata event table entries
    EVT_RADIOBUTTON( POLYDATA_RBUTTON,              Polydata::_onPolydata )
    EVT_CHECKBOX( POLYDATA_WARPED_SURFACE_CHK,      Polydata::_onWarpedSurface )
    EVT_CHECKBOX( POLYDATA_TWO_SIDED_LIGHTING_CHK,  Polydata::OnTwoSidedLightingChk )
    EVT_CHECKBOX( POLYDATA_GPU_TOOLS_CHK,           Polydata::_onAddPolydata )
    EVT_SLIDER( POLYDATA_PLANE_SLIDER,              Polydata::_onPolydataPlane )
    EVT_SLIDER( POLYDATA_OPACITY_SLIDER,            Polydata::OnPolydataOpacity )
    EVT_BUTTON( POLYDATA_ADD_POLYDATA_BUTTON,       Polydata::_onAddPolydata )
    EVT_BUTTON( POLYDATA_ADVANCED_POLYDATA_BUTTON,  Polydata::_onAdvanced )
    EVT_BUTTON( POLYDATA_SCALAR_CONTROL_BUTTON,     Polydata::OnScalarButton )
    ////@end polydata event table entries
END_EVENT_TABLE()
Polydata::Polydata( )
{}
//////////////////////////////////////////////////////////
Polydata::Polydata( wxWindow* parent, wxWindowID id,
                    const wxString& caption,
                    const wxPoint& pos,
                    const wxSize& size, long style )
{
    Create( parent, id, caption, pos, size, style );
    wxSize displaySize = ::wxGetDisplaySize();
    int tempH = displaySize.GetHeight() - 480;
    wxRect dialogPosition( displaySize.GetWidth() - 427, displaySize.GetHeight() - tempH, 427, tempH );
    this->SetSize( dialogPosition );
}
//////////////////////////////////////////////////////////
bool Polydata::Create( wxWindow* parent, wxWindowID id,
                       const wxString& caption,
                       const wxPoint& pos,
                       const wxSize& size, long style )
{
    _useWarpedSurfaceCheckBox = 0;
    _polydataSlider = 0;
    _advancedButton = 0;
    _computeButton = 0;
    m_opacitySlider = 0;
    m_twoSidedLighting = 0;
    m_particlesChk = 0;
    SetExtraStyle( GetExtraStyle() | wxWS_EX_BLOCK_EVENTS );
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit( this );
    GetSizer()->SetSizeHints( this );
    CentreOnParent();

    _polydataSlider->Enable( false );

    return true;
}
//////////////////////////////////
void Polydata::CreateControls()
{
    Polydata* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer( wxVERTICAL );
    itemDialog1->SetSizer( itemBoxSizer2 );

    wxStaticBox* itemStaticBoxSizer3Static = new wxStaticBox( itemDialog1, wxID_ANY, _T( "Polydata Controls" ) );
    wxStaticBoxSizer* itemStaticBoxSizer3 = new wxStaticBoxSizer( itemStaticBoxSizer3Static, wxVERTICAL );
    itemBoxSizer2->Add( itemStaticBoxSizer3, 0, wxGROW | wxALL, 5 );

    wxBoxSizer* topLevelControls = new wxBoxSizer( wxHORIZONTAL );

    _useWarpedSurfaceCheckBox = new wxCheckBox( itemDialog1, POLYDATA_WARPED_SURFACE_CHK, _T( "Use Warped Surface" ), wxDefaultPosition, wxDefaultSize, 0 );
    _useWarpedSurfaceCheckBox->SetValue( false );
    topLevelControls->Add( _useWarpedSurfaceCheckBox, 0, wxGROW | wxALL, 5 );
    
    m_particlesChk = new wxCheckBox( itemDialog1, POLYDATA_PARTICLES_CHK, _T( "Particles" ), wxDefaultPosition, wxDefaultSize, 0 );
    m_particlesChk->SetValue( false );
    topLevelControls->Add( m_particlesChk, 0, wxGROW | wxALL, 5 );
    
    itemStaticBoxSizer3->Add( topLevelControls, 0, wxGROW | wxALL, 5 );
    /////////////////////////////////////////
    wxStaticText* itemStaticText6 = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Scale Factor" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( itemStaticText6, 0, wxALIGN_LEFT | wxALL | wxADJUST_MINSIZE, 5 );

    _polydataSlider = new wxSlider( itemDialog1, POLYDATA_PLANE_SLIDER, 0, 0, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( _polydataSlider, 0, wxGROW | wxALL, 5 );
    /////////////////////////////////////////
    wxStaticBox* gpuToolsStaticSizer = new wxStaticBox( itemDialog1, wxID_ANY, _T( "GPU Tools" ) );
    wxStaticBoxSizer* itemStaticBoxSizer10 = new wxStaticBoxSizer( gpuToolsStaticSizer, wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemStaticBoxSizer10, 0, wxGROW | wxALL, 5 );
    
    m_gpuToolsChkBox = new wxCheckBox( itemDialog1, POLYDATA_GPU_TOOLS_CHK, _T( "Use GPU Tools" ), wxDefaultPosition, wxDefaultSize, 0 );
    m_gpuToolsChkBox->SetValue( false );
    itemStaticBoxSizer10->Add( m_gpuToolsChkBox, 0, wxALIGN_LEFT | wxALL, 5 );
    
    wxButton* scalarButton = new wxButton( itemDialog1, POLYDATA_SCALAR_CONTROL_BUTTON, _T( "Scalar Control" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer10->Add( scalarButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
    
    m_twoSidedLighting = new wxCheckBox( itemDialog1, POLYDATA_TWO_SIDED_LIGHTING_CHK, _T( "Two Sided Lighting" ), wxDefaultPosition, wxDefaultSize, 0 );
    m_twoSidedLighting->SetValue( false );
    itemStaticBoxSizer10->Add( m_twoSidedLighting, 0, wxALIGN_LEFT | wxALL, 5 );
    /////////////////////////////////////////
    wxStaticText* opacityStaticText = new wxStaticText( itemDialog1, wxID_STATIC, _T( "Opacity" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemStaticBoxSizer3->Add( opacityStaticText, 0, wxALIGN_LEFT | wxALL | wxADJUST_MINSIZE, 5 );
    
    m_opacitySlider = new wxSlider( itemDialog1, POLYDATA_OPACITY_SLIDER, 100, 0, 100, wxDefaultPosition, wxSize( 300, -1 ), wxSL_HORIZONTAL | wxSL_LABELS );
    itemStaticBoxSizer3->Add( m_opacitySlider, 0, wxGROW | wxALL, 5 );    
    ////////////////////////////////////////
    wxBoxSizer* itemBoxSizer8 = new wxBoxSizer( wxHORIZONTAL );
    itemStaticBoxSizer3->Add( itemBoxSizer8, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );

    _computeButton = new wxButton( itemDialog1, POLYDATA_ADD_POLYDATA_BUTTON, _T( "Update" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add( _computeButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    _advancedButton = new wxButton( itemDialog1, POLYDATA_ADVANCED_POLYDATA_BUTTON, _T( "Advanced..." ), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add( _advancedButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    wxButton* _closeButton = new wxButton( itemDialog1, wxID_OK, _T( "Close" ), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer8->Add( _closeButton, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );
}
///////////////////////////////////////////////////////////
void Polydata::SetActiveScalar( std::string activeScalar )
{
    _activeScalar = activeScalar;
}
////////////////////////////////////////////////////////////////
void Polydata::SetAvailableScalars( wxArrayString scalarNames )
{
    _scalarNames.Clear();
    for( size_t i = 0; i < scalarNames.Count(); i++ )
    {
        _scalarNames.Add( scalarNames[i] );
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
    wxUnusedVar( name );
    return wxNullBitmap;
}
////////////////////////////////////////////////////////////
wxIcon Polydata::GetIconResource( const wxString& name )
{
    wxUnusedVar( name );
    return wxNullIcon;
}
/////////////////////////////////////////////////////////
void Polydata::_onPolydata( wxCommandEvent& WXUNUSED( event ) )
{}
////////////////////////////////////////////////////////////////////
void Polydata::_onWarpedSurface( wxCommandEvent& WXUNUSED( event ) )
{
    if( _useWarpedSurfaceCheckBox->GetValue() == false )
    {
        _polydataSlider->Enable( false );
    }
    else if( _useWarpedSurfaceCheckBox->GetValue() == true )
    {
        _polydataSlider->Enable( true );
    }
}
/////////////////////////////////////////////////////////////
void Polydata::OnPolydataOpacity( wxCommandEvent& WXUNUSED( event ) )
{
    if( !m_gpuToolsChkBox->IsChecked() )
    {
        return;
    }
    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );
    newCommand->SetCommandName( "LIVE_POLYDATA_UPDATE" );
    
    ves::open::xml::DataValuePairPtr warpSurface( new ves::open::xml::DataValuePair() );
    warpSurface->SetData( "opacity", static_cast<double>(( m_opacitySlider->GetValue() ) ) );
    newCommand->AddDataValuePair( warpSurface );
    
    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( newCommand );
}
/////////////////////////////////////////////////////////////
void Polydata::_onPolydataPlane( wxCommandEvent& WXUNUSED( event ) )
{
    if( !_useWarpedSurfaceCheckBox->GetValue() && !m_gpuToolsChkBox->IsChecked() )
    {
        return;
    }
    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );
    newCommand->SetCommandName( "LIVE_POLYDATA_UPDATE" );

    ves::open::xml::DataValuePairPtr warpSurface( new ves::open::xml::DataValuePair() );
    warpSurface->SetData( "warpScale", static_cast<double>(( _polydataSlider->GetValue() ) ) );
    newCommand->AddDataValuePair( warpSurface );

    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( newCommand );
}
///////////////////////////////////////////////////////////
void Polydata::_onAddPolydata( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );
    newCommand->SetCommandName( "UPDATE_POLYDATA_SETTINGS" );

    if( m_particlesChk->GetValue() )
    {
        ves::open::xml::DataValuePairPtr particleViz( new ves::open::xml::DataValuePair() );
        particleViz->SetData( "Direction", "PARTICLE_VIZ" );
        newCommand->AddDataValuePair( particleViz );
    }

    ves::open::xml::DataValuePairPtr polydataValue( new ves::open::xml::DataValuePair() );
    polydataValue->SetData( "Polydata Value", static_cast<double>(( _polydataSlider->GetValue() ) ) );
    newCommand->AddDataValuePair( polydataValue );

    ves::open::xml::DataValuePairPtr colorByScalar( new ves::open::xml::DataValuePair() );
    colorByScalar->SetData( "Color By Scalar", _colorByScalarName );
    newCommand->AddDataValuePair( colorByScalar );

    ves::open::xml::DataValuePairPtr warpSurface( new ves::open::xml::DataValuePair() );
    warpSurface->SetDataName( "Warped Surface" );
    warpSurface->SetDataType( "UNSIGNED INT" );
    if( _useWarpedSurfaceCheckBox->GetValue() )
    {
        warpSurface->SetDataValue( static_cast<unsigned int>( 1 ) );
    }
    else
    {
        warpSurface->SetDataValue( static_cast<unsigned int>( 0 ) );
    }
    newCommand->AddDataValuePair( warpSurface );

    unsigned int checkBox = m_gpuToolsChkBox->IsChecked();
    ves::open::xml::DataValuePairPtr useGPUTools( new ves::open::xml::DataValuePair() );
    useGPUTools->SetData( "GPU Tools", checkBox );
    newCommand->AddDataValuePair( useGPUTools );
    
    try
    {
        dynamic_cast<Vistab*>( GetParent() )->SendUpdatedSettingsToXplorer( newCommand );
    }
    catch ( ... )
    {
            wxMessageBox( _( "Invalid Parent" ),
                          _( "Communication Failure" ),
                          wxOK | wxICON_INFORMATION );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Polydata::_onAdvanced( wxCommandEvent& WXUNUSED( event ) )
{
    int selectionIndex = 0;
    for( size_t i = 0; i < _scalarNames.Count(); i++ )
    {
        if( !_scalarNames[i].Cmp( wxString( _colorByScalarName.c_str(), wxConvUTF8 ) ) )
        {
            selectionIndex = i;
            break;
        }
    }
    wxSingleChoiceDialog scalarSelector( this, _T( "Select Scalar to color Polydata by." ), _T( "Color by Scalar" ),
                                         _scalarNames );

    scalarSelector.SetSize( GetRect() );
    scalarSelector.SetSelection( selectionIndex );
    scalarSelector.CentreOnParent();

    /*
       int displayWidth, displayHeight = 0;
       ::wxDisplaySize(&displayWidth,&displayHeight);

       wxRect bbox = GetRect();

       int width,height = 0;
       GetSize(&width,&height);
       scalarSelector.SetSize(wxRect( 2*displayWidth/3, bbox.GetBottomRight().y,
                            width, height));
       scalarSelector.SetSelection(selectionIndex);
    */

    if( scalarSelector.ShowModal() == wxID_OK )
    {
        _colorByScalarName = ConvertUnicode( scalarSelector.GetStringSelection() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Polydata::OnScalarButton( wxCommandEvent& WXUNUSED( event ) )
{
    PolyDataScalarControlDialog scalarDialog( this );
    scalarDialog.CentreOnParent();
    scalarDialog.ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void Polydata::OnTwoSidedLightingChk( wxCommandEvent& WXUNUSED( event ) )
{
    if( !m_gpuToolsChkBox->IsChecked() )
    {
        return;
    }
    ves::open::xml::CommandPtr newCommand( new ves::open::xml::Command() );
    newCommand->SetCommandName( "LIVE_POLYDATA_UPDATE" );
    
    ves::open::xml::DataValuePairPtr warpSurface( new ves::open::xml::DataValuePair() );
    warpSurface->SetData( "twoSidedLighting", static_cast<unsigned int>( m_twoSidedLighting->GetValue() ) );
    newCommand->AddDataValuePair( warpSurface );
    
    ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( newCommand );
}
////////////////////////////////////////////////////////////////////////////////
