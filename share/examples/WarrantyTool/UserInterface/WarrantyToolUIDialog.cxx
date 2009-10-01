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

#include <fstream>

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>

using namespace warrantytool;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
WarrantyToolUIDialog::WarrantyToolUIDialog()
    :
    mPartNumberEntry( 0 ),
    mServiceList( 0 ),
    MachineInfoDlg( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolUIDialog::WarrantyToolUIDialog( 
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
    :
    MachineInfoDlg( parent ),
    mPartNumberEntry( 0 ),
    mServiceList( service )
{    
    m_variableChoice01->Disable();
    m_variableLogicOperator01->Disable();
    m_textInput01->Disable();
    m_logicOperator01->Disable();
    m_variableChoice02->Disable();
    m_variableLogicOperator02->Disable();
    m_textInput02->Disable();
    m_logicOperator02->Disable();
    m_variableChoice03->Disable();
    m_variableLogicOperator03->Disable();
    m_textInput03->Disable();
    
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolUIDialog::~WarrantyToolUIDialog()
{
    /*Disconnect( GLOW_RESET, wxEVT_COMMAND_BUTTON_CLICKED,
            wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Disconnect( GLOW_CLEAR, wxEVT_COMMAND_BUTTON_CLICKED,
            wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Disconnect( GLOW_ADD, wxEVT_COMMAND_BUTTON_CLICKED,
            wxCommandEventHandler( WarrantyToolUIDialog::GetTextInput ) );
    Disconnect( OPEN_WARRANTY_FILE, wxEVT_COMMAND_BUTTON_CLICKED,
               wxCommandEventHandler( WarrantyToolUIDialog::OpenWarrantyFile ) );*/
}
////////////////////////////////////////////////////////////////////////////////
/*void WarrantyToolUIDialog::BuildGUI()
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
            this, wxID_ANY, wxT( "Part Number_static" ),
            wxDefaultPosition, wxDefaultSize, 0 );
            
    projectionEffectOpacityText->Wrap( -1 );
    projectionEffectOpacitySizer->Add( projectionEffectOpacityText, 1, wxALL, 5 );
        
    // add text input for axes
    //wxBoxSizer* axesTextBS = new wxBoxSizer( wxHORIZONTAL );
    //dataSetSBSizer->Add( axesTextBS, 0, wxGROW );
    mPartNumberEntry = new wxTextCtrl( this, wxID_ANY,//ID_DATA_UPDATE_AXES,
                                _( "Part Number_xtxt_crtl" ), wxDefaultPosition,
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
}*/
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
    size_t partNumberColumn = 0;

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
        boost::algorithm::trim( sCol1 );
        boost::algorithm::replace_all( sCol1, " ", "_" );
        data.push_back( sCol1 );
        csvDataMap[ columnCount ] = data;
        if( sCol1 == "Part_Number" )
        {
            partNumberColumn = columnCount;
        }
        columnCount += 1;
    }

    while( iss.good() )
    {
        std::getline(iss, sLine); // Get a line
        if( !iss.good() )
        {
            break;
        }

        if (sLine == "")
            continue;
        
        parser << sLine; // Feed the line to the parser
        //std::cout << sLine << std::endl;
        for( size_t i = 0; i < columnCount; ++i )
        {
            parser >> sCol1;
            csvDataMap[ i ].push_back( sCol1 );
        }
    }

    for( size_t i = 0; i < columnCount; ++i )
    {
        wxString columnNames = wxString( csvDataMap[ i ].at( 0 ).c_str(), wxConvUTF8 );
        //columnNames.Replace( wxT(" "), wxT("_") );
        m_columnStrings.Add( columnNames );
    }

    //iss.close();
    std::vector< std::string > prtnumbers = csvDataMap[ partNumberColumn ];
    //mPartNumberDescriptions = csvDataMap[ 3 ];
    //mLoadedPartNumbers = csvDataMap[ 2 ];
    //wxArrayString tempString;

    for( size_t i = 1; i < prtnumbers.size(); ++i )
    {
        m_partNumberStrings.Add( wxString( prtnumbers.at( i ).c_str(), wxConvUTF8 ) );
        //std::cout << prtnumbers.at( i ) << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnDataLoad( wxFileDirPickerEvent& event )
{
	// TODO: Implement OnDataLoad
    wxString fileName = event.GetPath();
    if( !fileName.IsEmpty() )
    {
        wxFileName viewPtsFilename( fileName );
        //viewPtsFilename.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
        wxString relativeViewLocationsPath( wxString( "./", wxConvUTF8 ) + 
                                           viewPtsFilename.GetFullPath() );
        
        DataValuePairPtr velFileName( new DataValuePair() );
        velFileName->SetData( "View Locations file", 
                             ConvertUnicode( relativeViewLocationsPath.c_str() ) );
        
        std::string csvFilename = 
        ConvertUnicode( viewPtsFilename.GetFullPath().c_str() );
        //Parse the csv file
        ParseDataFile( csvFilename );
        
        //tell ves to load
        ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
                                                                      new ves::open::xml::DataValuePair() );
        cameraGeometryOnOffDVP->SetData( "WARRANTY_FILE", csvFilename );
        ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
        command->AddDataValuePair( cameraGeometryOnOffDVP );
        std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
        command->SetCommandName( mCommandName );
        mServiceList->SendCommandStringToXplorer( command );
        
        //Populate all of the choice dialog boxes with the appropriate data
        m_variableChoice00->Append( m_columnStrings );
        m_variableChoice01->Append( m_columnStrings );
        m_variableChoice02->Append( m_columnStrings );
        m_variableChoice03->Append( m_columnStrings );
        
        m_manualPartSelectionChoice->Append( m_partNumberStrings );
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnVariableAndLogicalChoice( wxCommandEvent& event )
{
    wxChoice* eventChoice = dynamic_cast< wxChoice* >( event.GetEventObject() );
	// TODO: Implement OnVariableAndLogicalChoice

    //When the user selects a logical operator
    if( eventChoice == m_logicOperator00 )
    {
        //Turn on tools for 01
        //if off update query command
        if( m_logicOperator00->GetStringSelection() == wxString( "None", wxConvUTF8 ) )
        {
            m_variableChoice01->Disable();
            m_variableLogicOperator01->Disable();
            m_textInput01->Disable();
            m_logicOperator01->Disable();
            m_variableChoice02->Disable();
            m_variableLogicOperator02->Disable();
            m_textInput02->Disable();
            m_logicOperator02->Disable();
            m_variableChoice03->Disable();
            m_variableLogicOperator03->Disable();
            m_textInput03->Disable();
        }
        else
        {
            m_variableChoice01->Enable();
            m_variableLogicOperator01->Enable();
            m_textInput01->Enable();
            m_logicOperator01->Enable();            
        }
    }
    else if( eventChoice == m_logicOperator01 )
    {
        //Turn on tools for 02
        //if off update query command
        if( m_logicOperator01->GetStringSelection() == wxString( "None", wxConvUTF8 ) )
        {
            m_variableChoice02->Disable();
            m_variableLogicOperator02->Disable();
            m_textInput02->Disable();
            m_logicOperator02->Disable();
            m_variableChoice03->Disable();
            m_variableLogicOperator03->Disable();
            m_textInput03->Disable();
        }
        else
        {
            m_variableChoice02->Enable();
            m_variableLogicOperator02->Enable();
            m_textInput02->Enable();
            m_logicOperator02->Enable();
        }
    }
    else if( eventChoice == m_logicOperator02 )
    {
        //Turn on tools for 03
        //if off update query command
        if( m_logicOperator02->GetStringSelection() == wxString( "None", wxConvUTF8 ) )
        {
            m_variableChoice03->Disable();
            m_variableLogicOperator03->Disable();
            m_textInput03->Disable();
        }
        else
        {
            m_variableChoice03->Enable();
            m_variableLogicOperator03->Enable();
            m_textInput03->Enable();
        }
    }
    
    UpdateQueryDisplay();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnCreateInputText( wxCommandEvent& event )
{
	// TODO: Implement OnCreateInputText
    //Get the text from the user and update the query text display
    UpdateQueryDisplay();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnTextQueryEnter( wxCommandEvent& event )
{
	// TODO: Implement OnTextQueryEnter
    //When the user types in their on text entry submit the query
    SubmitQueryCommand();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnPartSelection( wxCommandEvent& event )
{
	// TODO: Implement OnPartSelection
    //When the user selects a part number submit it and update the associated 
    //text entry box
    m_partTextEntry->SetValue( m_manualPartSelectionChoice->GetStringSelection() );
    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );
    //mPartNumberList.push_back(
    //        ConvertUnicode( m_manualPartSelectionChoice->GetValue().c_str() ) );
    cameraGeometryOnOffDVP->SetData( "ADD", ConvertUnicode( m_manualPartSelectionChoice->GetStringSelection().c_str() ) );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnPartNumberEntry( wxCommandEvent& event )
{
	// TODO: Implement OnPartNumberEntry
    //When a user types in a part number to find submit it and go find it
        //mPartNumberList.push_back(
    //        ConvertUnicode( m_partTextEntry->GetValue().c_str() ) );
    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );
    cameraGeometryOnOffDVP->SetData( "ADD", ConvertUnicode( m_partTextEntry->GetValue().c_str() ) );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnQueryApply( wxCommandEvent& event )
{
	// TODO: Implement OnQueryApply
    //Submit the command currently in the query text box
    SubmitQueryCommand();    
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnDialogCancel( wxCommandEvent& event )
{
	// TODO: Implement OnDialogCancel
    //Do not do anything and close the dialog
    Close();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnQueryOK( wxCommandEvent& event )
{
	// TODO: Implement OnQueryOK
    //Submit the command currently in the query text box and close the dialog
    SubmitQueryCommand();
    Close();
}
////////////////////////////////////////////////////////////////////////////////
const std::string WarrantyToolUIDialog::GetTextFromChoice( wxChoice* variable,
                                                     wxChoice* logicOperator, wxTextCtrl* textInput )
{
    //"SELECT * FROM Parts WHERE Claims > 10"
    std::string variableString = ConvertUnicode( variable->GetStringSelection().c_str() );
    
    std::string logicString = ConvertUnicode( logicOperator->GetStringSelection().c_str() );
    if( logicString == "Less Than" )
    {
        logicString = "<";
    }
    else if( logicString == "Greater Than" )
    {
        logicString = ">";
    }
    else if( logicString == "Equal" )
    {
        logicString = "=";
    }
    else if( logicString == "Not Equal" )
    {
        logicString = "!=";
    }
    
    std::string inputString = ConvertUnicode( textInput->GetValue().c_str() );
    double tempData;
    if( !textInput->GetValue().ToDouble( &tempData ) )
    {
        inputString = "'" + inputString + "'";
    }
    std::string queryCommand = "\"" + variableString + "\" " + logicString + " " + inputString;
    
    return queryCommand;
}
////////////////////////////////////////////////////////////////////////////////
const std::string WarrantyToolUIDialog::GetTextFromLogicOperator( wxChoice* logicOperator )
{
    std::string logicString = ConvertUnicode( logicOperator->GetStringSelection().c_str() );
    if( logicString == "None" )
    {
        logicString = "";
    }
    else if( logicString == "And" )
    {
        logicString = "AND";
    }
    else if( logicString == "Or" )
    {
        logicString = "OR";
    }
    
    return logicString;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::SubmitQueryCommand()
{
    wxString queryText = m_queryTextCommandCtrl->GetValue();
    if( queryText.IsEmpty() )
    {
        return;
    }
    std::string queryString = ConvertUnicode( queryText.c_str() );
    
    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
                                                                  new ves::open::xml::DataValuePair() );
    cameraGeometryOnOffDVP->SetData( "QUERY_STRING", queryString );
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    std::string mCommandName = "WARRANTY_TOOL_DB_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );  
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::UpdateQueryDisplay()
{
    //Setup first variable
    std::string queryCommand = "SELECT * FROM Parts WHERE ";
    queryCommand += 
    GetTextFromChoice( m_variableChoice00, m_variableLogicOperator00, 
                      m_textInput00 );
    
    //Setup second variable
    std::string logicOperator;
    if( m_logicOperator00->IsEnabled() )
    {
        logicOperator = GetTextFromLogicOperator( m_logicOperator00 );
    }
    queryCommand = queryCommand + " " + logicOperator;
    
    if( m_variableChoice01->IsEnabled() )
    {
        queryCommand = queryCommand + " " +
        GetTextFromChoice( m_variableChoice01, 
                          m_variableLogicOperator01, m_textInput01 );
    }
    
    //Setup third variable
    logicOperator = "";
    if( m_logicOperator01->IsEnabled() )
    {
        logicOperator = GetTextFromLogicOperator( m_logicOperator01 );
    }
    queryCommand = queryCommand + " " + logicOperator;
    
    if( m_variableChoice02->IsEnabled() )
    {
        queryCommand = queryCommand + " " + 
        GetTextFromChoice( m_variableChoice02, 
                          m_variableLogicOperator02, m_textInput02 );
    }
    
    //Setup fourth variable
    logicOperator = "";
    if( m_logicOperator02->IsEnabled() )
    {
        logicOperator = GetTextFromLogicOperator( m_logicOperator02 );
    }
    queryCommand = queryCommand + " " + logicOperator;
    
    if( m_variableChoice03->IsEnabled() )
    {
        queryCommand = queryCommand + " " +
        GetTextFromChoice( m_variableChoice03, 
                          m_variableLogicOperator03, m_textInput03 );
    }
    
    //The update the text display box
    m_queryTextCommandCtrl->ChangeValue( wxString( queryCommand.c_str(), wxConvUTF8 ) );    
}
////////////////////////////////////////////////////////////////////////////////
