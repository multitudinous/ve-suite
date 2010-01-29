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
#include <ves/open/xml/OneDStringArray.h>

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

#include <Poco/SharedPtr.h>
#include <Poco/Tuple.h>
#include <Poco/Data/SessionFactory.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/SQLite/Connector.h>

using namespace Poco::Data;


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
    SetTitle( _("Deere Analytics Dialog") );
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolUIDialog::~WarrantyToolUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::SendCommandsToXplorer()
{
    ;
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

    unsigned int numStrings = m_displayTextChkList->GetCount();
    //wxArrayInt selections;
    //numStrings = m_displayTextChkList->GetSelections( selections );
    
    ves::open::xml::OneDStringArrayPtr textFields( 
        new ves::open::xml::OneDStringArray() );
    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( m_displayTextChkList->IsChecked( i ) )
        {
            textFields->AddElementToArray( 
                ConvertUnicode( m_displayTextChkList->GetString( i ).c_str() ) );
        }
    }
    ves::open::xml::DataValuePairPtr displayText( 
        new ves::open::xml::DataValuePair() );
    displayText->SetData( "DISPLAY_TEXT_FIELDS", textFields );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    command->AddDataValuePair( displayText );
    std::string mCommandName = "CAMERA_GEOMETRY_ON_OFF";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OpenWarrantyFile( wxCommandEvent& WXUNUSED( event ) )
{
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
        if( viewPtsFilename.GetExt() == _("csv") )
        {
            ParseDataFile( csvFilename );
        }
        else if( viewPtsFilename.GetExt() == _("db") )
        {
            ParseDataBase( csvFilename );
        }
        
        
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
        m_displayTextChkList->Append( m_columnStrings );

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
void WarrantyToolUIDialog::OnCreateInputText( wxCommandEvent& WXUNUSED( event ) )
{
	// TODO: Implement OnCreateInputText
    //Get the text from the user and update the query text display
    UpdateQueryDisplay();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnTextQueryEnter( wxCommandEvent& WXUNUSED( event ) )
{
	// TODO: Implement OnTextQueryEnter
    //When the user types in their on text entry submit the query
    SubmitQueryCommand();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnPartSelection( wxCommandEvent& WXUNUSED( event ) )
{
	// TODO: Implement OnPartSelection
    //When the user selects a part number submit it and update the associated 
    //text entry box
    m_partTextEntry->SetValue( m_manualPartSelectionChoice->GetStringSelection() );
    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );
    //mPartNumberList.push_back(
    //        ConvertUnicode( m_manualPartSelectionChoice->GetValue().c_str() ) );
    cameraGeometryOnOffDVP->SetData( "SCROLL", ConvertUnicode( m_manualPartSelectionChoice->GetStringSelection().c_str() ) );

    unsigned int numStrings = m_displayTextChkList->GetCount();
    //wxArrayInt selections;
    //numStrings = m_displayTextChkList->GetSelections( selections );
    
    ves::open::xml::OneDStringArrayPtr textFields( 
                                                  new ves::open::xml::OneDStringArray() );
    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( m_displayTextChkList->IsChecked( i ) )
        {
            textFields->AddElementToArray( 
                                          ConvertUnicode( m_displayTextChkList->GetString( i ).c_str() ) );
        }
    }
    ves::open::xml::DataValuePairPtr displayText( 
                                                 new ves::open::xml::DataValuePair() );
    displayText->SetData( "DISPLAY_TEXT_FIELDS", textFields );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    command->AddDataValuePair( displayText );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnPartNumberEntry( wxCommandEvent& WXUNUSED( event ) )
{
	// TODO: Implement OnPartNumberEntry
    //When a user types in a part number to find submit it and go find it
        //mPartNumberList.push_back(
    //        ConvertUnicode( m_partTextEntry->GetValue().c_str() ) );
    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );
    cameraGeometryOnOffDVP->SetData( "ADD", ConvertUnicode( m_partTextEntry->GetValue().c_str() ) );

    unsigned int numStrings = m_displayTextChkList->GetCount();
    //wxArrayInt selections;
    //numStrings = m_displayTextChkList->GetSelections( selections );
    
    ves::open::xml::OneDStringArrayPtr textFields( 
                                                  new ves::open::xml::OneDStringArray() );
    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( m_displayTextChkList->IsChecked( i ) )
        {
            textFields->AddElementToArray( 
                                          ConvertUnicode( m_displayTextChkList->GetString( i ).c_str() ) );
        }
    }
    ves::open::xml::DataValuePairPtr displayText( 
                                                 new ves::open::xml::DataValuePair() );
    displayText->SetData( "DISPLAY_TEXT_FIELDS", textFields );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    command->AddDataValuePair( displayText );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnTextChkListToggle( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateQueryDisplay();    
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnQueryApply( wxCommandEvent& WXUNUSED( event ) )
{
	// TODO: Implement OnQueryApply
    //Submit the command currently in the query text box
    SubmitQueryCommand();    
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnDialogCancel( wxCommandEvent& WXUNUSED( event ) )
{
	// TODO: Implement OnDialogCancel
    //Do not do anything and close the dialog
    Close();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnQueryOK( wxCommandEvent& WXUNUSED( event ) )
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
    else if( logicString == "Like" )
    {
        logicString = "LIKE";
    }
    
    std::string inputString = ConvertUnicode( textInput->GetValue().c_str() );
    double tempData;
    if( !textInput->GetValue().ToDouble( &tempData ) )
    {
        //If it is a string and we are doing a wild card search
        if( logicString == "LIKE" )
        {
            inputString = "'%" + inputString + "%'";
        }
        else
        {
            inputString = "'" + inputString + "'";
        }
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
    
    unsigned int numStrings = m_displayTextChkList->GetCount();
    //wxArrayInt selections;
    //numStrings = m_displayTextChkList->GetSelections( selections );
    
    ves::open::xml::OneDStringArrayPtr textFields( 
        new ves::open::xml::OneDStringArray() );
    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( m_displayTextChkList->IsChecked( i ) )
        {
            textFields->AddElementToArray( 
                ConvertUnicode( m_displayTextChkList->
                GetString( i ).c_str() ) );
        }
    }
    ves::open::xml::DataValuePairPtr displayText( 
        new ves::open::xml::DataValuePair() );
    displayText->SetData( "DISPLAY_TEXT_FIELDS", textFields );
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    command->AddDataValuePair( displayText );
    std::string mCommandName = "WARRANTY_TOOL_DB_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );  
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::UpdateQueryDisplay()
{
    //Setup first variable
    std::string queryCommand = "SELECT ";
    unsigned int numStrings = m_displayTextChkList->GetCount();
    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( m_displayTextChkList->IsChecked( i ) )
        {
            queryCommand += "\"" + ConvertUnicode( m_displayTextChkList->GetString( i ).c_str() ) + "\"" + ", ";
        }
    }
    queryCommand += "\"Part_Number\" ";
    queryCommand += "FROM Parts WHERE ";

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
void WarrantyToolUIDialog::ParseDataBase( const std::string& csvFilename )
{
    // register SQLite connector
    Poco::Data::SQLite::Connector::registerConnector();

    Poco::Data::Session session("SQLite", csvFilename );

    Statement select( session );
    
    ///////////////////////////////////////////
    //Get the column names
    try
    {
        //std::string queryString = "DESCRIBE Parts";
        std::string queryString = "SELECT * FROM Parts WHERE rowid = \"1\"";
        select << queryString.c_str(),now;
        //select.execute();
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText() << std::endl;
        return;
    }
    catch( Poco::Exception& ex )
    {
        std::cout << ex.displayText() << std::endl;
        return;
    }
    catch( ... )
    {
        std::cout << "UI Column name query is bad." << std::endl;
        return;
    }

	// create a RecordSet 
	Poco::Data::RecordSet rs(select);
	std::size_t cols = rs.columnCount();
    size_t numQueries = rs.rowCount();
    //std::cout << cols << " " << numQueries << std::endl;
    //std::cout <<  cols << " " << rs.columnName( 0 ) << std::endl;

	// iterate over all rows and columns
	/*bool more = false;
    try
    {
        more = rs.moveFirst();
    }
    catch( ... )
    {
        return;
    }*/
    for( size_t i = 0; i < cols; ++i )
    {
        wxString columnNames = wxString( rs.columnName( i ).c_str(), wxConvUTF8 );
        m_columnStrings.Add( columnNames );
    }
    /*while (more)
	{
        wxString columnNames = wxString( rs[0].convert<std::string>().c_str(), wxConvUTF8 );
        m_columnStrings.Add( columnNames );
        
		more = rs.moveNext();
	}*/
    ///////////////////////////////////////////

    ///////////////////////////////////////////
    //Get the partnumber from the db
    Statement select2( session );

    //select.reset( session );
    try
    {
        std::string queryString = "SELECT Part_Number FROM Parts";
        select2 << queryString.c_str(),now;
        //select.execute();
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText() << std::endl;
        return;
    }
    catch( ... )
    {
        std::cout << "UI Part Number query is bad." << std::endl;
        return;
    }

	// create a RecordSet 
	Poco::Data::RecordSet partRS(select2);
	cols = partRS.columnCount();
    numQueries = partRS.rowCount();
    
	// iterate over all rows and columns
    bool more = false;
    try
    {
        more = partRS.moveFirst();
    }
    catch( ... )
    {
        return;
    }

    while (more)
	{
        wxString partNames = wxString( partRS[0].convert<std::string>().c_str(), wxConvUTF8 );
        m_partNumberStrings.Add( partNames );
        
		more = partRS.moveNext();
	}

    try
    {
        Poco::Data::SQLite::Connector::unregisterConnector();
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText() << std::endl;
    }
    catch( Poco::AssertionViolationException& ex )
    {
        std::cout << ex.displayText() << std::endl;
    }    
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnToggleUnselected( wxCommandEvent& event )
{ 
    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
                                                                  new ves::open::xml::DataValuePair() );
    cameraGeometryOnOffDVP->SetData( "TOGGLE_PARTS", static_cast< unsigned int >( event.IsChecked() ) );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUIDialog::OnClearData( wxCommandEvent& WXUNUSED( event ) )
{ 
    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );
    //Clear all the glow
    cameraGeometryOnOffDVP->SetData( "CLEAR", "CLEAR" );
    mPartNumberList.clear();

    unsigned int numStrings = m_displayTextChkList->GetCount();
    //wxArrayInt selections;
    //numStrings = m_displayTextChkList->GetSelections( selections );

    ves::open::xml::OneDStringArrayPtr textFields( 
        new ves::open::xml::OneDStringArray() );

    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( m_displayTextChkList->IsChecked( i ) )
        {
            textFields->AddElementToArray( 
                                          ConvertUnicode( m_displayTextChkList->GetString( i ).c_str() ) );
        }
    }
    ves::open::xml::DataValuePairPtr displayText( 
                                                 new ves::open::xml::DataValuePair() );
    displayText->SetData( "DISPLAY_TEXT_FIELDS", textFields );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    command->AddDataValuePair( displayText );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
