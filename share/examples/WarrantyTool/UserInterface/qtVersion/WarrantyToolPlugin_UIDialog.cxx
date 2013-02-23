/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include "WarrantyToolPlugin_UIDialog.h"
#include "ui_WarrantyToolPlugin_UIDialog.h"
#include "QueryResults.h"

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/xplorer/command/CommandManager.h>
#include <ves/conductor/qt/UITabs.h>
#include <ves/conductor/qt/NaturalSortQTreeWidgetItem.h>
#include <ves/xplorer/scenegraph/Select.h>

#include "csvparser.h"

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/find.hpp>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

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

WarrantyToolPlugin_UIDialog::WarrantyToolPlugin_UIDialog(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::WarrantyToolPlugin_UIDialog),
    m_tableCounter(0),
    m_fileDialog(0)
{
    ui->setupUi(this);

    // Connect all the logic operators to a single slot which toggle sub-blocks
    // of the Query Composition group on and off.
    connect( ui->m_logicOperator00, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );
    connect( ui->m_logicOperator01, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );
    connect( ui->m_logicOperator02, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );

    // Likewise for variable choices...
    connect( ui->m_variableChoice00, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_variableChoiceS_changed(QString)) );
    connect( ui->m_variableChoice01, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_variableChoiceS_changed(QString)) );
    connect( ui->m_variableChoice02, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_variableChoiceS_changed(QString)) );
    connect( ui->m_variableChoice03, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_variableChoiceS_changed(QString)) );

    // ...and variable logic operators.
    connect( ui->m_variableLogicOperator00, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_variableLogicOperatorS_changed(QString)) );
    connect( ui->m_variableLogicOperator01, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_variableLogicOperatorS_changed(QString)) );
    connect( ui->m_variableLogicOperator02, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_variableLogicOperatorS_changed(QString)) );
    connect( ui->m_variableLogicOperator03, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_variableLogicOperatorS_changed(QString)) );

    // Text inputs too.
    connect( ui->m_textInput00, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput01, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput02, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput03, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));

    switchwire::EventManager* evm =
        switchwire::EventManager::instance();

    {
        std::string signalName = "WarrantyToolPlugin_UIDialog" +
            boost::lexical_cast<std::string>( this ) + ".ToggleUnselected";
        evm->RegisterSignal( ( &m_connectToggleUnselectedSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }

    {
        std::string signalName = "WarrantyToolPlugin_UIDialog" +
        boost::lexical_cast<std::string>( this ) + ".MouseSelection";
        evm->RegisterSignal( ( &m_connectMouseSelectionSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }

    {
        std::string signalName = "WarrantyToolPlugin_UIDialog" +
        boost::lexical_cast<std::string>( this ) + ".HighlightPart";
        evm->RegisterSignal( ( &m_highlightPartSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }

    {
        std::string signalName = "WarrantyToolPlugin_UIDialog" +
        boost::lexical_cast<std::string>( this ) + ".WarrantyToolHighlightParts";
        evm->RegisterSignal( ( &m_highlightPartsSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }
    
    {
        std::string signalName = "WarrantyToolPlugin_UIDialog" +
        boost::lexical_cast<std::string>( this ) + ".WarrantyTool.Clear";
        evm->RegisterSignal( ( &m_clearSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }

    {
        std::string signalName = "WarrantyToolPlugin_UIDialog" +
            boost::lexical_cast<std::string>( this ) + ".WarrantyTool.CustomQuery";
        evm->RegisterSignal( ( &m_querySignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }

    CONNECTSIGNALS_1( "%WarrantyTool.PartPicked",
                      void( const std::string& ),
                      &WarrantyToolPlugin_UIDialog::PartSelected,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_fileBrowseButton_clicked()
{
    ves::conductor::UITabs* tabs = ves::conductor::UITabs::instance();

    if( m_fileDialog )
    {
        tabs->ActivateTab( tabs->AddTab( m_fileDialog, "Select File" ) );
        return;
    }

    m_fileDialog = new QFileDialog( 0 );
    m_fileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    m_fileDialog->setAttribute( Qt::WA_DeleteOnClose );
    m_fileDialog->setFileMode( QFileDialog::ExistingFile );

    QStringList filters;
    filters << "SQLite DBs (*.db)" << "CSV files (*.csv)" << "All Files (*.*)";
    m_fileDialog->setNameFilters( filters );

    connect( m_fileDialog, SIGNAL(fileSelected(const QString &)),
                      this, SLOT(onFileSelected(const QString&)) );
    connect( m_fileDialog, SIGNAL(rejected()), this,
                      SLOT( onFileCancelled() ) );

    tabs->ActivateTab( tabs->AddTab( m_fileDialog, "Select File" ) );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::onFileSelected( const QString& filePath )
{

    QDir path = QDir::current();
    QString relativePath = path.relativeFilePath( filePath );
    ui->m_dataPath->setText( relativePath );

    ves::conductor::UITabs::instance()->RemoveTab( m_fileDialog );

    if ( m_fileDialog != 0 )
    {
        m_fileDialog->close();
        m_fileDialog = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::onFileCancelled()
{
    ves::conductor::UITabs::instance()->RemoveTab( m_fileDialog );

    if ( m_fileDialog != 0 )
    {
        m_fileDialog->close();
        m_fileDialog = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_dataLoadButton_clicked()
{
    QString path = ui->m_dataPath->text();
    if( !path.isEmpty() )
    {
        if( QFile::exists( path ) )
        {
            OnDataLoad( path.toStdString() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolPlugin_UIDialog::~WarrantyToolPlugin_UIDialog()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::changeEvent(QEvent *e)
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
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::m_logicOperatorS_currentIndexChanged ( QString const& text )
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
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::StripCharacters( std::string& data, const std::string& character )
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
void WarrantyToolPlugin_UIDialog::ParseDataFile( const std::string& csvFilename )
{
    // Clear out the variables and parts from any previously-loaded datafile
    m_columnStrings.clear();
    m_partNumberStrings.clear();

    std::string sLine;
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
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::OnDataLoad( std::string const& fileName )
{
    if( !fileName.empty() )
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
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::InputTextChanged( const QString& )
{
    UpdateQueryDisplay();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_queryTextCommandCtrl_returnPressed(  )
{
    //When the user types in their own text entry, submit the query
    SubmitQueryCommand();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_displayTextChkList_itemClicked( QListWidgetItem* )
{
    UpdateQueryDisplay();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_applyButton_clicked( )
{
    //Submit the command currently in the query text box
    SubmitQueryCommand();

    //Once we submit a query we can reset the table creation check box
    ui->m_createTableFromQuery->setChecked( false );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::m_variableChoiceS_changed(const QString &text)
{
    UpdateQueryDisplay();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::m_variableLogicOperatorS_changed(const QString &text)
{
    UpdateQueryDisplay();
}
////////////////////////////////////////////////////////////////////////////////
const std::string WarrantyToolPlugin_UIDialog::GetTextFromChoice( QComboBox* variable,
                                                     QComboBox* logicOperator, QLineEdit* textInput )
{
    //"SELECT * FROM Parts WHERE Claims > 10"
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
    }

    std::string queryCommand = "\"" + variableString + "\" " + logicString + " " + inputString;

    return queryCommand;
}
////////////////////////////////////////////////////////////////////////////////
const std::string WarrantyToolPlugin_UIDialog::GetTextFromLogicOperator( QComboBox* logicOperator )
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
void WarrantyToolPlugin_UIDialog::SubmitQueryCommand()
{

    QString queryText = ui->m_queryTextCommandCtrl->text();
    if( queryText.isEmpty() )
    {
        return;
    }
    std::string queryString = queryText.toStdString();

    QueryUserDefinedAndHighlightParts( queryString );

    // TODO: Can we jettison all remaining code in this method and just
    // uncomment the last line of QueryUserDefinedAndHighlightParts? Just
    // need to make sure the query code in GP isn't doing something extra
    // that we're not doing QueryUserDefinedAndHighlightParts.
    //ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
    //    new ves::open::xml::DataValuePair() );
    //cameraGeometryOnOffDVP->SetData( "QUERY_STRING", queryString );
    m_querySignal.signal( queryString );

    //unsigned int numStrings = ui->m_displayTextChkList->count();
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

    /*ves::open::xml::OneDStringArrayPtr textFields(
        new ves::open::xml::OneDStringArray() );
    for( unsigned int i = 0; i < numStrings; ++i )
    {
        if( ui->m_displayTextChkList->item(i)->checkState() == Qt::Checked )
        {
            textFields->AddElementToArray(
                ui->m_displayTextChkList->item( i )->text().toStdString() );
        }
    }*/
    /*ves::open::xml::DataValuePairPtr displayText(
        new ves::open::xml::DataValuePair() );
    displayText->SetData( "DISPLAY_TEXT_FIELDS", textFields );
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->AddDataValuePair( cameraGeometryOnOffDVP );
    command->AddDataValuePair( displayText );
    std::string mCommandName = "WARRANTY_TOOL_DB_TOOLS";
    command->SetCommandName( mCommandName );
    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );*/
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::UpdateQueryDisplay()
{
    std::string queryCommand;

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
    ui->m_queryTextCommandCtrl->setText( QString::fromStdString( queryCommand ) );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::ParseDataBase( const std::string& csvFilename )
{
    using namespace Poco::Data;
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
    ///////////////////////////////////////////
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_createTableFromQuery_toggled()
{
    //CREATE TABLE new_tbl SELECT * FROM orig_tbl;
    //http://dev.mysql.com/doc/refman/5.0/en/create-table-select.html
    //http://www.mydigitallife.info/2006/08/23/create-new-table-by-selecting-data-from-other-tables-with-create-table-as/
    UpdateQueryDisplay();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::QueryUserDefinedAndHighlightParts( const std::string& queryString )
{
    std::cout << "WarrantyToolPlugin_UIDialog::QueryUserDefinedAndHighlightParts " << queryString << std::endl << std::flush;
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
        std::cout << "WarrantyToolPlugin_UIDialog::QueryUserDefinedAndHighlightParts: exception A " << std::endl << std::flush;
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
            std::cout << "WarrantyToolPlugin_UIDialog::QueryUserDefinedAndHighlightParts: exception B " << std::endl << std::flush;
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

    QueryResults* resultsTab = new QueryResults( 0 );
    //QTreeWidget* queryResults = new QTreeWidget( 0 );
    //queryResults->setColumnCount( cols );

    std::string partNumberHeader;

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
    //queryResults->setHeaderLabels( headers );
    resultsTab->SetHeaders( headers );

    // Keep everything to the right of "WHERE " in the query to use as the tab
    // title
    QString title = QString::fromStdString( queryString );
    int wIndex = title.indexOf( "WHERE" );
    title = title.right( title.size() - (wIndex + 6 ) );

    /*ves::conductor::UITabs::instance()->
            ActivateTab( ves::conductor::UITabs::instance()->
                         AddTab( resultsTab, title.toStdString(), true ) );*/
    ui->m_tabWidget->addTab( resultsTab, title );

    std::vector<std::string> partNumbers;
    std::vector< QStringList > resultsData;

    while (more)
    {
        QStringList recordData;
        for (std::size_t col = 0; col < cols; ++col)
        {
            recordData.append( QString::fromStdString( rs[col].convert<std::string>() ) );
        }
        partNumbers.push_back( rs[0].convert<std::string>() );
        //This is not a memory leak because it is being add to the queryResults
        //QTreeWidget above.
        //QTreeWidgetItem* item = new ves::conductor::NaturalSortQTreeWidgetItem( queryResults, recordData );
        //item = 0;
        resultsData.push_back( recordData );

        more = rs.moveNext();
    }
    //queryResults->setSortingEnabled( true );
    resultsTab->PopulateResults( resultsData );

//    connect( queryResults, SIGNAL(currentItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)), this,
//             SLOT(QueryItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)));

    // I think this is all we need to do if we stop using the query code in the
    // GP -- RPT, 2013-01-15
    //m_highlightPartsSignal.signal( partNumbers );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::QueryItemChanged( QTreeWidgetItem* current,
                                                    QTreeWidgetItem* previous )
{
    std::string partNumber = current->text( current->columnCount() - 1 ).
            toStdString();
    m_highlightPartSignal.signal( partNumber );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_mouseSelection_clicked( bool checked )
{
    m_connectMouseSelectionSignal.signal( checked );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_toggleUnselected_clicked( bool checked )
{
    m_connectToggleUnselectedSignal.signal( checked );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_clear_clicked()
{
    m_clearSignal.signal();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::on_m_tabWidget_tabCloseRequested ( int index )
{
    if( index > 0 ) // don't allow "New Query" tab to be closed
    {
        delete ui->m_tabWidget->widget( index );
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolPlugin_UIDialog::PartSelected( const std::string& partNumber )
{
    if( !partNumber.empty() )
    {
        ui->m_selectedPartLabel->setText( QString::fromStdString( partNumber ) );
        QueryResults* queryTab = dynamic_cast< QueryResults* >( ui->m_tabWidget->currentWidget() );
        if( queryTab )
        {
            queryTab->SetSelectedPartNumber( partNumber );
        }
    }
}
