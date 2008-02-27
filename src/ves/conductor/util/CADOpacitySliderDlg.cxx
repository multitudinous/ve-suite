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
#include <ves/conductor/util/CADOpacitySliderDlg.h>
#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/msgdlg.h>

using namespace ves::open::xml::cad;
using namespace ves::conductor::util;
BEGIN_EVENT_TABLE( CADOpacitySliderDlg, wxDialog )
    EVT_COMMAND_SCROLL( OPACITY_SLIDER, CADOpacitySliderDlg::_onSlider )
END_EVENT_TABLE()
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
CADOpacitySliderDlg::CADOpacitySliderDlg( wxWindow* parent, int id,
                                          std::string cadNodeID,
                                          CADMaterialPtr material )
        : wxDialog( parent, id, _( "CADMaterial Opacity" ), wxDefaultPosition, wxDefaultSize,
                    ( wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX | wxMINIMIZE_BOX | wxCLOSE_BOX ), _( "CADMaterial Opacity" ) )
{
    _cadID = cadNodeID;
    _material = material;
    _buildDialog( _material->GetOpacity() );
}
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
CADOpacitySliderDlg::CADOpacitySliderDlg( wxWindow* parent, int id,
                                          std::string cadNodeID,
                                          float opacity )
        : wxDialog( parent, id, _( "CADMaterial Opacity" ), wxDefaultPosition, wxDefaultSize,
                    ( wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMAXIMIZE_BOX | wxMINIMIZE_BOX | wxCLOSE_BOX ), _( "CADMaterial Opacity" ) )
{
    _material = new CADMaterial();
    _cadID = cadNodeID;
    _buildDialog( opacity );
    CentreOnParent();
}
///////////////////////////////////////////
CADOpacitySliderDlg::~CADOpacitySliderDlg()
{}
/////////////////////////////////////////////////////
void CADOpacitySliderDlg::_buildDialog(float opacity)
{
    wxStaticBox* opacityGroup = new wxStaticBox( this, -1, wxT( "Opacity" ) );
    wxStaticBoxSizer* mainSizer = new wxStaticBoxSizer( opacityGroup, wxHORIZONTAL );

    wxBoxSizer* sliderSizer = new wxBoxSizer( wxHORIZONTAL );

    _opacitySlider = new wxSlider( this, OPACITY_SLIDER, 0 , 0, 100, wxDefaultPosition, wxDefaultSize,
                                   wxSL_HORIZONTAL | wxSL_AUTOTICKS | wxSL_LABELS );
    SetSliderValue( opacity );
    sliderSizer->Add( _opacitySlider, 1, wxALIGN_CENTER | wxEXPAND );

    mainSizer->Add( sliderSizer, 1, wxALIGN_CENTER | wxEXPAND );
    SetAutoLayout( true );
    SetSizer( mainSizer );
}
//////////////////////////////////////////////////////
void CADOpacitySliderDlg::SetSliderValue( double value )
{
    //This should be a pecentage that comes in so we convert it
    //to an int 0-100
    _opacitySlider->SetValue( static_cast<int>( 100 - 100*( 1.0 - value ) ) );
}
////////////////////////////////////////
double CADOpacitySliderDlg::GetOpacity()
{
    return ( double )( _opacitySlider->GetValue() ) / 100.0;
}
//////////////////////////////////////////////////////////
void CADOpacitySliderDlg::_onSlider( wxScrollEvent& WXUNUSED( event ) )
{
    //update the material
    //convert int to double

    //build the command
    _commandName = std::string( "CAD_OPACITY_UPDATE" );

    ves::open::xml::DataValuePairPtr nodeID( new ves::open::xml::DataValuePair() );
    nodeID->SetDataType( "STRING" );
    nodeID->SetData( std::string( "Node ID" ), _cadID );
    _instructions.push_back( nodeID );

    ves::open::xml::DataValuePairPtr opacityUpdate( new ves::open::xml::DataValuePair() );
    opacityUpdate->SetData( "Opacity Value", GetOpacity() );
    _instructions.push_back( opacityUpdate );

    _sendCommandsToXplorer();

    _clearInstructions();
}
//////////////////////////////////////////////
void CADOpacitySliderDlg::_clearInstructions()
{
    _instructions.clear();
    _commandName.clear() ;
}
//////////////////////////////////////////////////
void CADOpacitySliderDlg::_sendCommandsToXplorer()
{
    ves::open::xml::CommandPtr opacityCommand( new ves::open::xml::Command() );

    for( size_t i = 0; i < _instructions.size(); i++ )
    {
        opacityCommand->AddDataValuePair( _instructions.at( i ) );
    }
    opacityCommand->SetCommandName( _commandName );

    {
        try
        {
            ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer( opacityCommand );
        }
        catch ( ... )
        {
            wxMessageBox( _( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect." ),
                          _( "Communication Failure" ), wxOK | wxICON_INFORMATION );
        }
    }
    //Clean up memory
    _clearInstructions();
}

