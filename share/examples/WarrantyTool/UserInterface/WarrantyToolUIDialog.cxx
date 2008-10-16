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
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "WarrantyToolUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/conductor/util/spinctld.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/statline.h>
#include <wx/sizer.h>
#include <wx/radiobox.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/statbox.h>
#include <wx/frame.h>
#include <wx/textctrl.h>
#include <wx/notebook.h>

using namespace warrantytool;

BEGIN_EVENT_TABLE( WarrantyToolUIDialog, wxDialog )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
WarrantyToolUIDialog::WarrantyToolUIDialog()
    :
    UIDialog(),
    mPartNumberEntry( 0 ),
    mServiceList( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolUIDialog::WarrantyToolUIDialog( 
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
    :
    UIDialog( parent, id, wxT( "WarrantyTool" ) ),
    mPartNumberEntry( 0 ),
    mServiceList( service )
{    
    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolUIDialog::~WarrantyToolUIDialog()
{
    Disconnect( GLOW_RESET, wxEVT_COMMAND_BUTTON_CLICKED,
            wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Disconnect( GLOW_CLEAR, wxEVT_COMMAND_BUTTON_CLICKED,
            wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Disconnect( GLOW_ADD, wxEVT_COMMAND_BUTTON_CLICKED,
            wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::BuildGUI()
{
    SetSizeHints( wxDefaultSize, wxDefaultSize );
    SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    wxBoxSizer* mainSizer = 0;
    mainSizer = new wxBoxSizer( wxVERTICAL );

    ///////////////////////////////////////////////////////////
    wxBoxSizer* projectionEffectOpacitySizer = 0;
    projectionEffectOpacitySizer = new wxBoxSizer( wxVERTICAL );
    mainSizer->Add( projectionEffectOpacitySizer, 1, wxGROW );

    wxStaticText* projectionEffectOpacityText = 0;
    projectionEffectOpacityText = new wxStaticText(
            this, wxID_ANY, wxT( "Part Number" ),
            wxDefaultPosition, wxDefaultSize, 0 );
            
    projectionEffectOpacityText->Wrap( -1 );
    projectionEffectOpacitySizer->Add( projectionEffectOpacityText, 1, wxALL, 5 );
        
    // add text input for axes
    //wxBoxSizer* axesTextBS = new wxBoxSizer( wxHORIZONTAL );
    //dataSetSBSizer->Add( axesTextBS, 0, wxGROW );
    mPartNumberEntry = new wxTextCtrl( this, wxID_ANY,//ID_DATA_UPDATE_AXES,
                                _( "X Axis" ), wxDefaultPosition,
                                wxDefaultSize, wxHSCROLL | wxTE_PROCESS_ENTER );
    projectionEffectOpacitySizer->Add( mPartNumberEntry, 1, wxALL, 5 );
    //xAxisEntry->Raise();
    
    //Put the buttons on
    //wxStdDialogButtonSizer* stdDialogButtonSizer;
    //stdDialogButtonSizer = new wxStdDialogButtonSizer();
    //stdDialogButtonSizer->Realize();
    wxBoxSizer* buttonSizer = 0;
    buttonSizer = new wxBoxSizer( wxHORIZONTAL );
    //Ok
    {
        wxButton* stdDialogButtonOK = 0;
        stdDialogButtonOK = new wxButton( this, wxID_OK );
        buttonSizer->Add( stdDialogButtonOK, 0, wxALL, 5 );
    }
    //Cancel
    {
        wxButton* stdDialogButtonCancel = 0;
        stdDialogButtonCancel = new wxButton( this, wxID_CANCEL );
        buttonSizer->Add( stdDialogButtonCancel, 0, wxALL, 5 );
    }
    //Reset
    {
        wxButton* stdDialogButtonReset = 0;
        stdDialogButtonReset = new wxButton( this, GLOW_RESET, _("Reset") );
        buttonSizer->Add( stdDialogButtonReset, 0, wxALL, 5 );
    }
    //Clear
    {
        wxButton* stdDialogButtonClear = 0;
        stdDialogButtonClear = new wxButton( this, GLOW_CLEAR, _("Clear") );
        buttonSizer->Add( stdDialogButtonClear, 0, wxALL, 5 );
    }
    //Add
    {
        wxButton* stdDialogButtonAdd = 0;
        stdDialogButtonAdd = new wxButton( this, GLOW_ADD, _("Add") );
        buttonSizer->Add( stdDialogButtonAdd, 0, wxALL, 5 );
    }

    mainSizer->Add( buttonSizer, 0, wxALL | wxEXPAND, 5 );
    
    Connect( GLOW_RESET, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Connect( GLOW_CLEAR, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Connect( GLOW_ADD, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    ///////////////////////////////////////////////////////////

    SetSizer( mainSizer );
    Layout();
    mainSizer->Fit( this );
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 

    /*for( size_t i = 0; i < mInstructions.size(); ++i )
    {
        command->AddDataValuePair( mInstructions.at( i ) );
    }

    command->SetCommandName( mCommandName );*/

    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::GetTextInput( wxCommandEvent& event )
{
    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );

    if( event.GetId() == GLOW_RESET )
    {
        //Clear glow and make opaque
        cameraGeometryOnOffDVP->SetData( "RESET", "RESET" );
        mPartNumberList.clear();
    }
    else if( event.GetId() == GLOW_CLEAR )
    {
        //Clear all the glow
        cameraGeometryOnOffDVP->SetData( "CLEAR", "CLEAR" );
        mPartNumberList.clear();
    }
    else if( event.GetId() == GLOW_ADD )
    {
        //If add is pushed then send the name to add
        mPartNumberList.push_back( 
            ConvertUnicode( mPartNumberEntry->GetValue().c_str() ) );
        cameraGeometryOnOffDVP->SetData( "ADD", ConvertUnicode( mPartNumberEntry->GetValue().c_str() ) );
    }

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    std::string mCommandName = "CAMERA_GEOMETRY_ON_OFF";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}