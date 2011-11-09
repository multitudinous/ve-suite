/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include "PaintingPlugin_UIDialog.h"
#include "ui_PaintingPlugin_UIDialog.h"

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/eventmanager/EventManager.h>

#include <ves/conductor/qt/UITabs.h>

//#include "csvparser.h"

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/find.hpp>

#include <Poco/Data/SQLite/SQLite.h>
#include <Poco/Data/SQLite/Connector.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/Statement.h>
#include <Poco/Data/RecordSet.h>

#include <QtCore/QString>
#include <QtGui/QTreeWidget>
#include <QtGui/QTreeWidgetItem>

#include <string>
#include <vector>
#include <map>
#include <fstream>

PaintingPlugin_UIDialog::PaintingPlugin_UIDialog(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::PaintingPlugin_UIDialog),
    m_tableCounter(0)
{
    ui->setupUi(this);

    // Connect all the logic operators to a single slot which toggle sub-blocks
    // of the Query Composition group on and off.
    /*connect( ui->m_logicOperator00, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );
    connect( ui->m_logicOperator01, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );
    connect( ui->m_logicOperator02, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );

    connect( ui->m_textInput00, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput01, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput02, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput03, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));*/

    ves::xplorer::eventmanager::EventManager* evm =
        ves::xplorer::eventmanager::EventManager::instance();
    using ves::xplorer::eventmanager::SignalWrapper;
    
    {
        std::string signalName = "PaintingPlugin_UIDialog" +
            boost::lexical_cast<std::string>( this ) + ".ConnectToSensorServer";
        evm->RegisterSignal(
            new SignalWrapper< ves::util::TwoStringSignal_type >( &m_connectSensorSignal ),
            signalName, ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::on_m_sensorClientConnect_clicked()
{
    std::cout << "get ip address and send it to xplorer" << std::endl;
    std::cout << ui->m_heaterClientIP->text().toStdString() << " " 
        << ui->m_sensorClientIP->text().toStdString() << " " 
        << ui->m_sensorPort->text().toStdString() << " "
        << ui->m_heaterPort->text().toStdString() << std::endl;
    m_connectSensorSignal( ui->m_sensorClientIP->text().toStdString(), ui->m_sensorPort->text().toStdString() );
}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::on_m_heaterClientConnect_clicked()
{
    std::cout << "get ip address and send it to xplorer" << std::endl;
    std::cout << ui->m_heaterClientIP->text().toStdString() << " " 
        << ui->m_sensorClientIP->text().toStdString() << " " 
        << ui->m_sensorPort->text().toStdString() << " "
        << ui->m_heaterPort->text().toStdString() << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::on_m_testTableView_clicked()
{
std::cout << "here 1 " << std::endl;
    QTreeWidget* queryResults = new QTreeWidget( 0 );
    //QTreeWidget* queryResults = new QTreeWidget( ui->m_sensorData );
    
    queryResults->setColumnCount( 2 );
    
    std::string partNumber;
    std::string partNumberHeader;
    std::string partText;
    
    // Get header names
    QStringList headers;
    headers.append( QString::fromStdString( "Sensor Number" ) );
    headers.append( QString::fromStdString( "Sensor Type" ) );

    /*if( more )
    {
        for (std::size_t col = 0; col < cols; ++col)
        {
            partNumberHeader = rs.columnName( col );
            headers.append( QString::fromStdString( partNumberHeader ));
        }
    }*/
    
    queryResults->setHeaderLabels( headers );
    
    // Keep everything to the right of "WHERE " in the query to use as the tab
    // title
    QString title = QString::fromStdString( "Sensor Information" );
    
    ves::conductor::UITabs::instance()->
    ActivateTab( ves::conductor::UITabs::instance()->
                AddTab( queryResults, title.toStdString() ) );
    
    /*while (more)
    {
        QStringList recordData;
        for (std::size_t col = 0; col < cols; ++col)
        {
            recordData.append( QString::fromStdString( rs[col].convert<std::string>() ) );
        }
        QTreeWidgetItem* item = new QTreeWidgetItem( queryResults, recordData );
        
        more = rs.moveNext();
    }*/
    
    ///
    {
        QStringList recordData;
        recordData.append( QString::fromStdString( "1" ) );
        recordData.append( QString::fromStdString( "Thermocouple" ) );
        QTreeWidgetItem* item = new QTreeWidgetItem( queryResults, recordData );
    }
    ///
    {
        QStringList recordData;
        recordData.append( QString::fromStdString( "2" ) );
        recordData.append( QString::fromStdString( "Super Thermocouple" ) );
        QTreeWidgetItem* item = new QTreeWidgetItem( queryResults, recordData );
    }
    
    queryResults->setSortingEnabled( true );
}
////////////////////////////////////////////////////////////////////////////////
PaintingPlugin_UIDialog::~PaintingPlugin_UIDialog()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////

void PaintingPlugin_UIDialog::changeEvent(QEvent *e)
{
    QWidget::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}
/*
void PaintingPlugin_UIDialog::m_logicOperatorS_currentIndexChanged ( QString const& text )
{
    QObject* caller = sender();
    if( caller == ui->m_logicOperator00 )
    {
        if( text == "None" )
        {
            ui->m_logicGroup01->setEnabled( false );
        }
        else
        {
            ui->m_logicGroup01->setEnabled( true );
        }
    }
    else if( caller == ui->m_logicOperator01 )
    {
        if( text == "None" )
        {
            ui->m_logicGroup02->setEnabled( false );
        }
        else
        {
            ui->m_logicGroup02->setEnabled( true );
        }
    }
    else if( caller == ui->m_logicOperator02 )
    {
        if( text == "None" )
        {
            ui->m_logicGroup03->setEnabled( false );
        }
        else
        {
            ui->m_logicGroup03->setEnabled( true );
        }
    }

    //UpdateQueryDisplay();
}
*/

////////////////////////////////////////////////////////////////////////////////
//void PaintingPlugin_UIDialog::SendCommandsToXplorer()
//{
//    ;
//}
////////////////////////////////////////////////////////////////////////////////
// Can't find any place this is called, so commenting out. -RPT
//void PaintingPlugin_UIDialog::GetTextInput( wxCommandEvent& event )
//{
//    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
//        new ves::open::xml::DataValuePair() );

//    if( event.GetId() == GLOW_RESET )
//    {
//        //Clear glow and make opaque
//        cameraGeometryOnOffDVP->SetData( "RESET", "RESET" );
//        mPartNumberList.clear();
//    }
//    else if( event.GetId() == GLOW_CLEAR )
//    {
//        //Clear all the glow
//        cameraGeometryOnOffDVP->SetData( "CLEAR", "CLEAR" );
//        mPartNumberList.clear();
//        m_queryTextCommandCtrl->ChangeValue( _("") );
//    }
//    else if( event.GetId() == GLOW_ADD )
//    {
//        //If add is pushed then send the name to add
//        /*mPartNumberList.push_back(
//            ConvertUnicode( mPartNumberEntry->GetValue().c_str() ) );
//        cameraGeometryOnOffDVP->SetData( "ADD", ConvertUnicode( mPartNumberEntry->GetValue().c_str() ) );*/
//        mPartNumberList.push_back(
//            ConvertUnicode( m_partListCombo->GetValue().c_str() ) );
//        cameraGeometryOnOffDVP->SetData( "ADD", ConvertUnicode( m_partListCombo->GetValue().c_str() ) );
//    }

//    unsigned int numStrings = m_displayTextChkList->GetCount();
//    //wxArrayInt selections;
//    //numStrings = m_displayTextChkList->GetSelections( selections );

//    ves::open::xml::OneDStringArrayPtr textFields(
//        new ves::open::xml::OneDStringArray() );
//    for( unsigned int i = 0; i < numStrings; ++i )
//    {
//        if( m_displayTextChkList->IsChecked( i ) )
//        {
//            textFields->AddElementToArray(
//                ConvertUnicode( m_displayTextChkList->GetString( i ).c_str() ) );
//        }
//    }
//    ves::open::xml::DataValuePairPtr displayText(
//        new ves::open::xml::DataValuePair() );
//    displayText->SetData( "DISPLAY_TEXT_FIELDS", textFields );

//    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
//    command->AddDataValuePair( cameraGeometryOnOffDVP );
//    command->AddDataValuePair( displayText );
//    std::string mCommandName = "CAMERA_GEOMETRY_ON_OFF";
//    command->SetCommandName( mCommandName );
//    mServiceList->SendCommandStringToXplorer( command );
//}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::StripCharacters( std::string& data, const std::string& character )
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
void PaintingPlugin_UIDialog::ParseDataFile( const std::string& csvFilename )
{
    /*std::string sLine;
    std::string sCol1, sCol3, sCol4;
    //double fCol2;
    //int iCol5, iCol6;

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
        m_columnStrings.append( QString::fromStdString( csvDataMap[ i ].at( 0 ) ) );
    }

    std::vector< std::string > prtnumbers = csvDataMap[ partNumberColumn ];
    for( size_t i = 1; i < prtnumbers.size(); ++i )
    {
        m_partNumberStrings.append( QString::fromStdString( prtnumbers.at( i ) ) );
        //std::cout << prtnumbers.at( i ) << std::endl;
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::OnDataLoad( std::string const& fileName )
{
    /*if( !fileName.empty() )
    {
        boost::filesystem::path viewPtsFilename( fileName );
        std::string relativeViewLocationsPath(  "./" +
                                           viewPtsFilename.string() );

        ves::open::xml::DataValuePairPtr velFileName( new ves::open::xml::DataValuePair() );
        velFileName->SetData( "View Locations file",
                              relativeViewLocationsPath );

        std::string csvFilename =  viewPtsFilename.string();
        m_filename = csvFilename;
        //Parse the csv file
        if( viewPtsFilename.extension().string() == (".csv") )
        {
            ParseDataFile( csvFilename );
        }
        else if( viewPtsFilename.extension().string() == (".CSV") )
        {
            ParseDataFile( csvFilename );
        }
        else if( viewPtsFilename.extension().string() == (".db") )
        {
            ParseDataBase( csvFilename );
        }
        else if( viewPtsFilename.extension().string() == (".DB") )
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
        ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );

        //Populate all of the choice dialog boxes with the appropriate data
        ui->m_variableChoice00->addItems( m_columnStrings );
        ui->m_variableChoice01->addItems( m_columnStrings );
        ui->m_variableChoice02->addItems( m_columnStrings );
        ui->m_variableChoice03->addItems( m_columnStrings );
        ui->m_displayTextChkList->addItems( m_columnStrings );
        for( int index = 0; index < ui->m_displayTextChkList->count(); ++index )
        {
            ui->m_displayTextChkList->item(index)->setCheckState( Qt::Unchecked );
        }
        QList<QListWidgetItem *> partNums =
                ui->m_displayTextChkList->findItems( "part_number", Qt::MatchFixedString );
        if( !partNums.empty() )
        {
            delete partNums.at( 0 );
        }

        ui->m_manualPartSelectionChoice->addItems( m_partNumberStrings );
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::InputTextChanged( const QString& text )
{
    // TODO: Implement OnCreateInputText
    //Get the text from the user and update the query text display
    UpdateQueryDisplay();
}

////////////////////////////////////////////////////////////////////////////////
/*void PaintingPlugin_UIDialog::on_m_queryTextCommandCtrl_returnPressed(  )
{
    //When the user types in their on text entry submit the query
    SubmitQueryCommand();
}*/
/*
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::OnPartSelection( wxCommandEvent& WXUNUSED( event ) )
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
void PaintingPlugin_UIDialog::OnPartNumberEntry( wxCommandEvent& WXUNUSED( event ) )
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
*/
/*void PaintingPlugin_UIDialog::on_m_displayTextChkList_itemClicked
        ( QListWidgetItem* item )
{
    UpdateQueryDisplay();
}*/
////////////////////////////////////////////////////////////////////////////////


void PaintingPlugin_UIDialog::on_m_applyButton_clicked( )
{
    //Submit the command currently in the query text box
    SubmitQueryCommand();

    //Once we submit a query we can reset the table creation check box
    //ui->m_createTableFromQuery->setChecked( false );
}



/*
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::OnDialogCancel( wxCommandEvent& WXUNUSED( event ) )
{
    // TODO: Implement OnDialogCancel
    //Do not do anything and close the dialog
    Close();
}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::OnQueryOK( wxCommandEvent& WXUNUSED( event ) )
{
    //Submit the command currently in the query text box and close the dialog
    SubmitQueryCommand();

    //Once we submit a query we can reset the table creation check box
    m_createTableFromQuery->SetValue( false );

    Close();
}
*/
////////////////////////////////////////////////////////////////////////////////
const std::string PaintingPlugin_UIDialog::GetTextFromChoice( QComboBox* variable,
                                                     QComboBox* logicOperator, QLineEdit* textInput )
{
    /*//"SELECT * FROM Parts WHERE Claims > 10"
    //std::string variableString = ConvertUnicode( variable->GetStringSelection().c_str() );
    std::string variableString = variable->currentText().toStdString();

    std::string logicString = logicOperator->currentText().toStdString();
    const std::string uiLogicString = logicString;
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
    else if( logicString == "Not Like" )
    {
        logicString = "NOT LIKE";
    }
    else if( logicString == "Begins With" )
    {
        logicString = "LIKE";
    }
    else if( logicString == "Does Not Begin With" )
    {
        logicString = "NOT LIKE";
    }

    std::string inputString = textInput->text().toStdString();
    bool tempData;
    if( !textInput->text().toDouble( &tempData ) )
    {
        //If it is a string and we are doing a wild card search
        if( uiLogicString == "Like" )
        {
            inputString = "'%" + inputString + "%'";
        }
        else if( uiLogicString == "Not Like" )
        {
            inputString = "'%" + inputString + "%'";
        }
        else if( uiLogicString == "Begins With" )
        {
            inputString = "'" + inputString + "%'";
        }
        else if( uiLogicString == "Does Not Begin With" )
        {
            inputString = "'" + inputString + "%'";
        }
        else
        {
            inputString = "'" + inputString + "'";
        }
    }*/

    std::string queryCommand;// = "\"" + variableString + "\" " + logicString + " " + inputString;

    return queryCommand;
}
////////////////////////////////////////////////////////////////////////////////
const std::string PaintingPlugin_UIDialog::GetTextFromLogicOperator( QComboBox* logicOperator )
{
    std::string logicString = logicOperator->currentText().toStdString();
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
void PaintingPlugin_UIDialog::SubmitQueryCommand()
{
    /*QString queryText = ui->m_queryTextCommandCtrl->text();
    if( queryText.isEmpty() )
    {
        return;
    }
    std::string queryString = queryText.toStdString();

    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );
    cameraGeometryOnOffDVP->SetData( "QUERY_STRING", queryString );

    unsigned int numStrings = ui->m_displayTextChkList->count();
    //wxArrayInt selections;
    //numStrings = m_displayTextChkList->GetSelections( selections );

    //Setup the table controls
    if( ui->m_createTableFromQuery->isChecked() )
    {
        std::string tableName = "User_Table_" +
            boost::lexical_cast<std::string>( m_tableCounter );
        m_tableCounter += 1;
        m_tableList.push_back( tableName );

        QString tempTableName = QString::fromStdString( tableName );
        ui->m_tableChoice1->addItem( tempTableName );
        ui->m_tableChoice2->addItem( tempTableName );
        ui->m_tableChoice3->addItem( tempTableName );
        ui->m_tableChoice4->addItem( tempTableName );
    }

    ves::open::xml::OneDStringArrayPtr textFields(
        new ves::open::xml::OneDStringArray() );
    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( ui->m_displayTextChkList->item(i)->checkState() == Qt::Checked )
        {
            textFields->AddElementToArray(
                ui->m_displayTextChkList->item( i )->text().toStdString() );
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
    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );

    QueryUserDefinedAndHighlightParts( queryString );*/
}

////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::UpdateQueryDisplay()
{
    /*std::string queryCommand;

    if( ui->m_createTableFromQuery->isChecked() )
    {
        std::string tableName = "User_Table_" + boost::lexical_cast<std::string>( m_tableCounter );
        queryCommand = "CREATE TABLE " + tableName + " AS SELECT ";
    }
    else
    {
        queryCommand = "SELECT ";
    }
    //Setup first variable
    unsigned int numStrings = ui->m_displayTextChkList->count();
    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( ui->m_displayTextChkList->item( i )->checkState() == Qt::Checked )
        {
            queryCommand += "\"" + ( ui->m_displayTextChkList->item( i )->text().toStdString() )
                            + "\"" + ", ";
        }
    }
    queryCommand += "\"Part_Number\" ";
    queryCommand += "FROM Parts WHERE ";

    queryCommand +=
        GetTextFromChoice( ui->m_variableChoice00, ui->m_variableLogicOperator00,
                      ui->m_textInput00 );

    //Setup second variable
    std::string logicOperator;
    if( ui->m_logicOperator00->isEnabled() )
    {
        logicOperator = GetTextFromLogicOperator( ui->m_logicOperator00 );
    }
    queryCommand = queryCommand + " " + logicOperator;

    if( ui->m_variableChoice01->isEnabled() )
    {
        queryCommand = queryCommand + " " +
        GetTextFromChoice( ui->m_variableChoice01,
                          ui->m_variableLogicOperator01, ui->m_textInput01 );
    }

    //Setup third variable
    logicOperator = "";
    if( ui->m_logicOperator01->isEnabled() )
    {
        logicOperator = GetTextFromLogicOperator( ui->m_logicOperator01 );
    }
    queryCommand = queryCommand + " " + logicOperator;

    if( ui->m_variableChoice02->isEnabled() )
    {
        queryCommand = queryCommand + " " +
        GetTextFromChoice( ui->m_variableChoice02,
                          ui->m_variableLogicOperator02, ui->m_textInput02 );
    }

    //Setup fourth variable
    logicOperator = "";
    if( ui->m_logicOperator02->isEnabled() )
    {
        logicOperator = GetTextFromLogicOperator( ui->m_logicOperator02 );
    }
    queryCommand = queryCommand + " " + logicOperator;

    if( ui->m_variableChoice03->isEnabled() )
    {
        queryCommand = queryCommand + " " +
        GetTextFromChoice( ui->m_variableChoice03,
                          ui->m_variableLogicOperator03, ui->m_textInput03 );
    }

    //The update the text display box
    ui->m_queryTextCommandCtrl->setText( QString::fromStdString( queryCommand ) );*/
}
////////////////////////////////////////////////////////////////////////////////


void PaintingPlugin_UIDialog::ParseDataBase( const std::string& csvFilename )
{
    /*using namespace Poco::Data;
    // register SQLite connector
    Poco::Data::SQLite::Connector::registerConnector();

    Poco::Data::Session session("SQLite", csvFilename );

    Poco::Data::Statement select( session );

    ///////////////////////////////////////////
    //Get the column names
    {
        try
        {
            //std::string queryString = "DESCRIBE Parts";
            std::string queryString = "SELECT * FROM Parts WHERE rowid = \"1\"";
            select << queryString, now;
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
        //size_t numQueries = rs.rowCount();

        for( size_t i = 0; i < cols; ++i )
        {
            m_columnStrings.append( QString::fromStdString( rs.columnName( i ) ) );
        }
    }
    ///////////////////////////////////////////

    ///////////////////////////////////////////
    //Get the partnumber from the db
    {
        Statement select2( session );
        try
        {
            std::string queryString = "SELECT Part_Number FROM Parts";
            select2 << queryString.c_str(),now;
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
        //std::size_t cols = partRS.columnCount();
        //std::size_t numQueries = partRS.rowCount();

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
            m_partNumberStrings.append( QString::fromStdString( partRS[0].convert<std::string>() ) );

            more = partRS.moveNext();
        }
    }
    ///////////////////////////////////////////

    ///////////////////////////////////////////
    //Figure out how many tables are present in the database
    {
        Statement select3( session );
        try
        {
            std::string queryString = "SELECT name FROM sqlite_master WHERE tbl_name LIKE 'User_Table_%'";
            select3 << queryString.c_str(),now;
        }
        catch( Poco::Data::DataException& ex )
        {
            std::cout << ex.displayText() << std::endl;
            return;
        }
        catch( ... )
        {
            std::cout << "UI Table Number query is bad." << std::endl;
            return;
        }

        // create a RecordSet
        Poco::Data::RecordSet tableRS(select3);
        //std::size_t cols = tableRS.columnCount();
        //std::size_t numQueries = tableRS.rowCount();

        // iterate over all rows and columns
        bool more = false;
        try
        {
            more = tableRS.moveFirst();
        }
        catch( ... )
        {
            return;
        }

        while (more)
        {
            std::string tableName = tableRS[0].convert<std::string>();
            m_tableCounter += 1;
            m_tableList.push_back( tableName );

            QString tempTableName( QString::fromStdString( tableName ) );
            ui->m_tableChoice1->addItem( tempTableName );
            ui->m_tableChoice2->addItem( tempTableName );
            ui->m_tableChoice3->addItem( tempTableName );
            ui->m_tableChoice4->addItem( tempTableName );
            more = tableRS.moveNext();
        }
    }
    ///////////////////////////////////////////

    ///////////////////////////////////////////
    //Shutdown the database
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
    ///////////////////////////////////////////*/
}
/*
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::OnToggleUnselected( wxCommandEvent& event )
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
void PaintingPlugin_UIDialog::OnClearData( wxCommandEvent& WXUNUSED( event ) )
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

    //Send commands to clear the user selected tables
    m_tableList.clear();
    m_tableCounter = 0;
    m_tableChoice1->Clear();
    m_tableChoice2->Clear();
    m_tableChoice3->Clear();
    m_tableChoice4->Clear();

    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
*/
/*void PaintingPlugin_UIDialog::on_m_createTableFromQuery_toggled()
{
    //CREATE TABLE new_tbl SELECT * FROM orig_tbl;
    //http://dev.mysql.com/doc/refman/5.0/en/create-table-select.html
    //http://www.mydigitallife.info/2006/08/23/create-new-table-by-selecting-data-from-other-tables-with-create-table-as/
    UpdateQueryDisplay();
}*/
////////////////////////////////////////////////////////////////////////////////
/*
void PaintingPlugin_UIDialog::OnTableSelection( wxCommandEvent& WXUNUSED( event ) )
{
    //SELECT Artists.ArtistName, CDs.Title FROM Artists INNER JOIN CDs ON Artists.ArtistID=CDs.ArtistID;
    int selectedChoice = m_tableChoice1->GetSelection();
    if( selectedChoice == wxNOT_FOUND )
    {
        return;
    }

    selectedChoice = m_tableChoice1->GetSelection();
    if( selectedChoice == wxNOT_FOUND )
    {
        return;
    }

    std::string choice1 =
        ConvertUnicode( m_tableChoice1->GetStringSelection().c_str() );
    std::string choice2 =
        ConvertUnicode( m_tableChoice2->GetStringSelection().c_str() );

    if( choice1 == choice2 )
    {
        return;
    }

    std::string queryCommand;
    queryCommand = "SELECT * FROM ";
    queryCommand += choice1;
    queryCommand += " INNER JOIN " + choice2;
    queryCommand += " ON ";
    queryCommand += choice2 + ".Part_Number=" + choice1 + ".Part_Number";

    m_queryTextCommandCtrl->ChangeValue( wxString( queryCommand.c_str(), wxConvUTF8 ) );

    if( choice1.empty() || choice2.empty() )
    {
        return;
    }

    //Send xplorer the table names that are currently active
    ves::open::xml::OneDStringArrayPtr tableArray(
        new ves::open::xml::OneDStringArray() );
    tableArray->AddElementToArray( choice1 );
    tableArray->AddElementToArray( choice2 );

    ves::open::xml::DataValuePairPtr tableDVP(
        new ves::open::xml::DataValuePair() );
    tableDVP->SetData( "SET_ACTIVE_TABLES", tableArray );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->AddDataValuePair( tableDVP );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::OnMouseSelection( wxCommandEvent& event )
{
    ves::open::xml::DataValuePairSharedPtr
        cameraGeometryOnOffDVP( new ves::open::xml::DataValuePair() );
    cameraGeometryOnOffDVP->
        SetData( "MOUSE_SELECTION",
        static_cast< unsigned int >( event.IsChecked() ) );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void PaintingPlugin_UIDialog::OnSaveQuery( wxCommandEvent& event )
{
    wxFileDialog dialog( NULL, _T( "Save Current Query..." ),
                        ::wxGetCwd(),
                        _T( "db_query1" ),
                        _T( "txt (*.txt)|*.txt" ),
                        wxFD_SAVE | wxFD_OVERWRITE_PROMPT
                        );

    if( dialog.ShowModal() != wxID_OK )
    {
        return;
    }
    wxFileName vesFileName;
    vesFileName = dialog.GetPath();
    std::string filename = ConvertUnicode( vesFileName.GetFullPath().c_str() );

    ves::open::xml::DataValuePairSharedPtr
        cameraGeometryOnOffDVP( new ves::open::xml::DataValuePair() );
    cameraGeometryOnOffDVP->
        SetData( "SAVE", filename );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    std::string mCommandName = "WARRANTY_TOOL_PART_TOOLS";
    command->SetCommandName( mCommandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
*/
void PaintingPlugin_UIDialog::QueryUserDefinedAndHighlightParts( const std::string& queryString )
{
    /*std::cout << "PaintingPlugin_UIDialog::QueryUserDefinedAndHighlightParts " << queryString << std::endl << std::flush;
    //select << "SELECT Part_Number, Description, Claims FROM Parts",
    //select << "SELECT Part_Number, Description, Claims FROM Parts WHERE Claims > 10 AND Claims_Cost > 1000",
    Poco::Data::Session session("SQLite", m_filename );
    Poco::Data::Statement select( session );
    try
    {
        select << queryString , Poco::Data::now;
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << "PaintingPlugin_UIDialog::QueryUserDefinedAndHighlightParts: exception A " << std::endl << std::flush;
        std::cout << ex.displayText() << std::endl;
        return;
    }
    catch( ... )
    {
        return;
    }


    if( !queryString.compare( 0, 12, "CREATE TABLE" ) )
    {
        //boost::find_first( queryString, " AS " );
        boost::iterator_range<std::string::const_iterator>::iterator stringIter = boost::find_first( queryString, " AS " ).begin();
        //std::cout << stringIter - queryString.begin() << std::endl;
        //Get first position for string above
        std::size_t numEntries = ( stringIter - queryString.begin() ) + 4;
        std::string tempString = queryString;
        //Remove from begining to position of string above
        tempString.erase( 0, numEntries );
        //std::cout << tempString << std::endl;
        //Now rerun select query and color by new color
        //return;
        try
        {
            Poco::Data::Statement select2( session );
            select2 << tempString.c_str(), Poco::Data::now;
            select.swap( select2 );
        }
        catch( Poco::Data::DataException& ex )
        {
            std::cout << "PaintingPlugin_UIDialog::QueryUserDefinedAndHighlightParts: exception B " << std::endl << std::flush;
            std::cout << ex.displayText() << std::endl;
            return;
        }
    }
    // create a RecordSet
    Poco::Data::RecordSet rs(select);
    std::size_t cols = rs.columnCount();
    size_t numQueries = rs.rowCount();
    std::ostringstream outString;
    outString << "Number of parts found " << numQueries;
    if( numQueries == 0 )
    {
        return;
    }
    // iterate over all rows and columns
    bool more = false;
    try
    {
        more = rs.moveFirst();
    }
    catch( ... )
    {
        return;
    }

    QTreeWidget* queryResults = new QTreeWidget( 0 );
    queryResults->setColumnCount( cols );

    std::string partNumber;
    std::string partNumberHeader;
    std::string partText;

    // Get header names
    QStringList headers;
    if( more )
    {
        for (std::size_t col = 0; col < cols; ++col)
        {
            partNumberHeader = rs.columnName( col );
            headers.append( QString::fromStdString( partNumberHeader ));
        }
    }
    queryResults->setHeaderLabels( headers );

    // Keep everything to the right of "WHERE " in the query to use as the tab
    // title
    QString title = QString::fromStdString( queryString );
    int wIndex = title.indexOf( "WHERE" );
    title = title.right( title.size() - (wIndex + 6 ) );

    ves::conductor::UITabs::instance()->
            ActivateTab( ves::conductor::UITabs::instance()->
                         AddTab( queryResults, title.toStdString() ) );

    while (more)
    {
        QStringList recordData;
        for (std::size_t col = 0; col < cols; ++col)
        {
            recordData.append( QString::fromStdString( rs[col].convert<std::string>() ) );
        }
        QTreeWidgetItem* item = new QTreeWidgetItem( queryResults, recordData );

        more = rs.moveNext();
    }
    queryResults->setSortingEnabled( true );*/
}
