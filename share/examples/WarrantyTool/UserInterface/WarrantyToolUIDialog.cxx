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

// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "WarrantyToolUIDialog.h"
#include "wxFixWidthImportCtrl.h"
#include "csvparser.h"

// --- VE-Suite Includes --- //
#include <ves/conductor/util/spinctld.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

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
#include <wx/combobox.h>

#include <wx/filename.h>
#include <wx/choicdlg.h>

#include <wx/filedlg.h>
#include <wx/textdlg.h>

using namespace warrantytool;
using namespace ves::open::xml;

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
    Disconnect( OPEN_WARRANTY_FILE, wxEVT_COMMAND_BUTTON_CLICKED,
               wxCommandEventHandler( WarrantyToolUIDialog::OpenWarrantyFile ) );
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
                                _( "Part Number" ), wxDefaultPosition,
                                wxDefaultSize, wxTE_PROCESS_ENTER );
    projectionEffectOpacitySizer->Add( mPartNumberEntry, 1, wxALL, 5 );
    mPartNumberEntry->Raise();
    
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
    
    wxBoxSizer* warrantyDialogSizer = 0;
    warrantyDialogSizer = new wxBoxSizer( wxHORIZONTAL );

    //Open
    {
        wxButton* stdDialogButtonAdd = 0;
        stdDialogButtonAdd = new wxButton( this, OPEN_WARRANTY_FILE, _("Open") );
        warrantyDialogSizer->Add( stdDialogButtonAdd, 0, wxALL, 5 );
    }
    //Warranty file
    {
        //mTabDialog = new wxFixWidthImportCtrl( this, OPEN_WARRANTY_FILE );
        //buttonSizer->Add( mTabDialog, 0, wxALL, 5 );
        wxArrayString choices;
        mPartListCMB = new wxComboBox( this, 
                   PART_SELECTION, 
                   wxString(), 
                   wxDefaultPosition, 
                   wxDefaultSize, 
                   choices, 
                   wxCB_DROPDOWN, 
                   wxDefaultValidator );
        warrantyDialogSizer->Add( mPartListCMB, 0, wxALL, 5 );
   }

    mainSizer->Add( warrantyDialogSizer, 0, wxALL | wxEXPAND, 5 );
    mainSizer->Add( buttonSizer, 0, wxALL | wxEXPAND, 5 );

    Connect( GLOW_RESET, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Connect( GLOW_CLEAR, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Connect( GLOW_ADD, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Connect( OPEN_WARRANTY_FILE, wxEVT_COMMAND_BUTTON_CLICKED,
        wxCommandEventHandler( WarrantyToolUIDialog::OpenWarrantyFile ) );
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
        /*mPartNumberList.push_back( 
            ConvertUnicode( mPartNumberEntry->GetValue().c_str() ) );
        cameraGeometryOnOffDVP->SetData( "ADD", ConvertUnicode( mPartNumberEntry->GetValue().c_str() ) );*/
        mPartNumberList.push_back( 
            ConvertUnicode( mPartListCMB->GetValue().c_str() ) );
        cameraGeometryOnOffDVP->SetData( "ADD", ConvertUnicode( mPartListCMB->GetValue().c_str() ) );
    }

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    std::string mCommandName = "CAMERA_GEOMETRY_ON_OFF";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OpenWarrantyFile( wxCommandEvent& event )
{
    //wxString fileName( _("C:/dev/test_data/tab_delimited/0904-RIDesktop.txt") );
    //mTabDialog->SetTabSize( 7 );
    //mTabDialog->LoadFile( fileName );
    wxFileDialog dialog( this,
                        _T( "Open File" ),
                        ::wxGetCwd(),
                        _T( "" ),
                        _T( "Warranty data file (*.csv;*.tsv)|*.csv;*.tsv;" ),
                        wxOPEN | wxFILE_MUST_EXIST | wxFD_PREVIEW,
                        wxDefaultPosition );
    dialog.CentreOnParent();
    
    if( dialog.ShowModal() == wxID_OK )
    {
        wxFileName viewPtsFilename( dialog.GetPath() );
        //viewPtsFilename.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
        wxString relativeViewLocationsPath( wxString( "./", wxConvUTF8 ) + viewPtsFilename.GetFullPath() );
        
        DataValuePairPtr velFileName( new DataValuePair() );
        velFileName->SetData( "View Locations file", ConvertUnicode( relativeViewLocationsPath.c_str() ) );
        //_dataValuePairList.push_back( velFileName );
        
        ///_commandName = "QC_LOAD_STORED_POINTS";
        //SendCommandsToXplorer();
        std::string csvFilename = ConvertUnicode( viewPtsFilename.GetFullPath().c_str() );
        ParseDataFile( csvFilename );
        
        ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
            new ves::open::xml::DataValuePair() );
        cameraGeometryOnOffDVP->SetData( "WARRANTY_FILE", csvFilename );
        ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
        command->AddDataValuePair( cameraGeometryOnOffDVP );
        std::string mCommandName = "CAMERA_GEOMETRY_ON_OFF";
        command->SetCommandName( mCommandName );
        mServiceList->SendCommandStringToXplorer( command );        
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::StripCharacters( std::string& data, const std::string& character )
{
    for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( character, index );
        if ( index != std::string::npos )
        {
            data.replace( index, 1, "\n" );
            //data.erase( index, 1 );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::ParseDataFile( const std::string& csvFilename )
{
    std::string sLine;
    std::string sCol1, sCol3, sCol4;
    double fCol2;
    int iCol5, iCol6;

    CSVParser parser;

    std::ifstream infile( csvFilename.c_str() );
    //std::streampos beforeNetwork;
    //beforeNetwork = inFile.tellg();

    infile.seekg( 0, ios::end);
    std::streampos length = infile.tellg();
    infile.seekg (0, ios::beg);

    //Now we have passed the network data so record it
    //std::streampos afterNetwork;
    //afterNetwork = inFile.tellg();
    //go back to the beginning of the network
    //inFile.seekg( beforeNetwork );
    // allocate memory:
    char* buffer = new char [ length ];
    // read data as a block:
    infile.read( buffer, (length) );
    //std::ofstream tester4 ("tester4.txt");
    //tester4<<buffer<<std::endl;
    //tester4.close();
    infile.close();

    std::string networkData( buffer );
    delete [] buffer;
    StripCharacters( networkData, "\r" );

    //std::cout << networkData << std::endl;
    //build network information
    //CreateNetworkInformation( networkData );
    std::istringstream iss;
    iss.str( networkData );

    std::getline(iss, sLine); // Get a line
    parser << sLine; // Feed the line to the parser
    size_t columnCount = 0;
    std::map< int, std::vector< std::string > > csvDataMap;

    while( parser.getPos() < sLine.size() )
    {
        parser >> sCol1; // Feed the line to the parser
        //std::cout << sCol1 << std::endl;
        std::vector< std::string > data;
        if( sCol1.empty() )
        {
            std::ostringstream headerTemp;
            headerTemp << "N/A " << columnCount;
            sCol1 = headerTemp.str();
        }
        data.push_back( sCol1 );
        //std::vector< std::string > data;
        csvDataMap[ columnCount ] = data;
        columnCount += 1;
    }

    while (!iss.eof()) 
    {
        std::getline(iss, sLine); // Get a line
        if (sLine == "")
            continue;
        
        parser << sLine; // Feed the line to the parser
        std::cout << sLine << std::endl;
        for( size_t i = 0; i < columnCount; ++i )
        {
            parser >> sCol1;
            csvDataMap[ i ].push_back( sCol1 );
        }
    }
    //iss.close();
    std::vector< std::string > prtnumbers = csvDataMap[ 2 ];
    mPartNumberDescriptions = csvDataMap[ 3 ];
    mLoadedPartNumbers = csvDataMap[ 2 ];
    wxArrayString tempString;

    for( size_t i = 0; i < prtnumbers.size(); ++i )
    {
        tempString.Add( wxString( prtnumbers.at( i ).c_str(), wxConvUTF8 ) );
        //std::cout << prtnumbers.at( i ) << std::endl;
    }
    mPartListCMB->Append( tempString );
}
