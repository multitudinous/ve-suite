// --- This class --- //
#include "DynamicVehicleSimTool_UIDialog.h"
#include "ui_DynamicVehicleSimTool_UIDialog.h"

// --- Qt includes --- //
#include <QtGui/QHBoxLayout>
#include <QtGui/QLabel>


// --- VES includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/model/Model.h>
#include <ves/conductor/qt/UITabs.h>

#include <switchwire/EventManager.h>
//#include <switchwire/OptionalMacros.h>

// --- Local plugin includes --- //
#include "CADListCreator.h"

////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimTool_UIDialog::DynamicVehicleSimTool_UIDialog(QWidget* parent, ves::conductor::UIPluginBase* pluginBase) :
    QWidget(parent),
    m_uiPluginBase( pluginBase ),
    ui(new Ui::DynamicVehicleSimTool_UIDialog)
{
    ui->setupUi(this);

    // Signal registration
    switchwire::EventManager* evm = switchwire::EventManager::instance();
    evm->RegisterSignal( &m_setComputerDetails, "DynSim.SetComputerDetails" );
    evm->RegisterSignal( &m_setSimState, "DynSim.SetSimState" );
    evm->RegisterSignal( &m_setSimScale, "DynSim.SetSimScale" );
    evm->RegisterSignal( &m_setGeometryMap, "DynSim.SetGeometryMap" );
    evm->RegisterSignal( &m_setGeometryConstraint, "DynSim.SetGeometryConstraint" );
    evm->RegisterSignal( &m_registrationUpdate, "DynSim.RegistrationUpdate" );
    evm->RegisterSignal( &m_simReset, "DynSim.SimReset" );
}
////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimTool_UIDialog::~DynamicVehicleSimTool_UIDialog()
{
    delete ui;
}

bool DynamicVehicleSimTool_UIDialog::TransferDataToWindow()
{
    PopulateDialogs();
    return true;
}

bool DynamicVehicleSimTool_UIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_startStopButton_clicked()
{
    double scaleValue = 1.0;
    //("m -> ft"), ("cm -> ft"), ("mm -> ft"), ("in -> ft")
    int choice = ui->m_unitsSelector->currentIndex();
    if( choice == 0 )
    {
        scaleValue = 3.2808399;
    }
    else if( choice == 1 )
    {
        scaleValue = 0.032808399;
    }
    else if( choice == 2 )
    {
        scaleValue = 0.0032808399;
    }
    else if( choice == 3 )
    {
        scaleValue = 0.0833333;
    }

    m_setSimScale.signal( scaleValue );

    bool state = ui->m_startStopButton->isChecked();
    std::string simState = "Stop";
    if( state )
    {
        on_m_computerName_textChanged( "" );
        simState = "Start";
    }

    m_setSimState.signal( simState );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_resetButton_clicked()
{
    m_simReset.signal();

    //Reset it back to start
    bool state = ui->m_startStopButton->isChecked();
    std::string simState = "Stop";
    if( state )
    {
        simState = "Start";
    }

    m_setSimState.signal( simState );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_addButton_clicked()
{
    size_t newNum = m_geomChoiceList.size();
    QHBoxLayout* layout = new QHBoxLayout( 0 );

    QString num;
    num.setNum( newNum );
    QLabel* label = new QLabel( num, this );
    layout->addWidget( label );

    ves::open::xml::cad::CADNodePtr rootNode = m_uiPluginBase->GetVEModel()->GetGeometry();
    dynamicvehicletool::CADListCreator nodeListCreator( rootNode );
    m_nodeList = nodeListCreator.GetNodeList();
    std::vector< std::string > nodeListNames =
        nodeListCreator.GetNodeNameList();
    QStringList choices;
    if( nodeListNames.size() != m_nodeList.size() )
    {
        std::cout << " something is wrong with name generation." << std::endl;
    }

    for( size_t i = 0; i < nodeListNames.size(); ++i )
    {
        choices.append( QString::fromStdString( nodeListNames.at( i ) ) );
    }
    QComboBox* choice = new QComboBox( this );
    choice->addItems( choices );
    choice->setCurrentIndex( 0 );
    layout->addWidget( choice );

    // I can't see that this checkbox is used anywhere, so omitting it for now --RPT
//    wxCheckBox* checkBox = new wxCheckBox( m_scrolledWindow1, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
//    bSizer->Add( checkBox, 0, wxALL, 5 );

    ui->m_choiceListLayout->addLayout( layout );
    m_geomChoiceList.push_back( choice );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_removeButton_clicked()
{
    // Remove the last item;
    QLayoutItem* item = ui->m_choiceListLayout->takeAt(
                                          ui->m_choiceListLayout->count() - 1 );
    if( item )
    {
        delete item;
    }
    m_geomChoiceList.pop_back();

    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_applyButton_clicked()
{
    on_m_userConstrainedGeometrySelector_currentIndexChanged( -1 );

    std::vector< std::string > geometryVector;
    if( m_nodeList.size() > 0 )
    {
        for( size_t i = 0; i < m_geomChoiceList.size(); ++i )
        {
            ves::open::xml::cad::CADNodePtr tempCADNode =
                m_nodeList.at( m_geomChoiceList.at( i )->currentIndex() );
            geometryVector.push_back( tempCADNode->GetID() );
        }
    }
    if( (m_geomChoiceList.size() == 0) || (m_nodeList.size() == 0) )
    {
        geometryVector.push_back( "No Geom" );
    }

    m_setGeometryMap.signal( geometryVector );

    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_registerButton_clicked()
{
    std::string mode = ui->m_registrationSelector->currentText().toStdString();

    double sipX = 0.0;
    double sipY = 0.0;
    double sipZ = 0.0;

    sipX = ui->m_SIPX->text().toDouble();
    sipY = ui->m_SIPY->text().toDouble();
    sipZ = ui->m_SIPZ->text().toDouble();

    std::vector< double > sipLocations;
    sipLocations.push_back( sipX );
    sipLocations.push_back( sipY );
    sipLocations.push_back( sipZ );

    std::string forward;
    std::string up;

    forward = ui->m_CADForwardVector->currentText().toStdString();
    up = ui->m_CADUpVector->currentText().toStdString();

    m_registrationUpdate.signal( mode, m_fileName, sipLocations, forward, up );

    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_computerName_textChanged(const QString& )
{
    m_setComputerDetails.signal( ui->m_computerName->text().toStdString(),
                                 ui->m_portNumber->text().toStdString() );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_portNumber_textChanged(const QString& )
{
    on_m_computerName_textChanged( "" );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_userConstrainedGeometrySelector_currentIndexChanged( int )
{
    // We ignore the passed index because we call this method from other places
    // in our code without testing the index.
    int selection = ui->m_userConstrainedGeometrySelector->currentIndex();
    std::string geometryID;
    if( ui->m_userConstrainedGeometrySelector->currentIndex() == 0 )
    {
        geometryID = "None";
    }
    else
    {
        ves::open::xml::cad::CADNodePtr tempCADNode = m_nodeList.at(
                                                                selection - 1 );
        geometryID = tempCADNode->GetID();
    }

    m_setGeometryConstraint.signal( geometryID );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::on_m_registrationSelector_currentIndexChanged( int index )
{
    if( index == 0 )
    {
        m_fileName = "";
    }
    else if( index == 1 )
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
        filters << "All Files (*.*)";
        m_fileDialog->setNameFilters( filters );

        connect( m_fileDialog, SIGNAL(fileSelected(const QString &)),
                          this, SLOT(onFileSelected(const QString&)) );
        connect( m_fileDialog, SIGNAL(rejected()), this,
                          SLOT( onFileCancelled() ) );

        tabs->ActivateTab( tabs->AddTab( m_fileDialog, "Select File" ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::onFileSelected( const QString& filePath )
{

    QDir path = QDir::current();
    QString relativePath = path.relativeFilePath( filePath );
    m_fileName = relativePath.toStdString();

    ves::conductor::UITabs::instance()->RemoveTab( m_fileDialog );

    if ( m_fileDialog != 0 )
    {
        m_fileDialog->close();
        m_fileDialog = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::onFileCancelled()
{
    ves::conductor::UITabs::instance()->RemoveTab( m_fileDialog );

    if ( m_fileDialog != 0 )
    {
        m_fileDialog->close();
        m_fileDialog = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::UpdateModelData()
{
    ves::open::xml::model::ModelPtr tempModel = m_uiPluginBase->GetVEModel();

    ves::open::xml::CommandPtr toolCommand( new ves::open::xml::Command() );
    toolCommand->SetCommandName( "Tool Info" );

    {
        ves::open::xml::DataValuePairPtr constrainedText( new ves::open::xml::DataValuePair() );
        if( ui->m_userConstrainedGeometrySelector->currentIndex() == 0 )
        {
            constrainedText->SetData( "Constrained Geometry", "None" );
        }
        else
        {
            ves::open::xml::cad::CADNodePtr tempCADNode = m_nodeList.at(
                    ui->m_userConstrainedGeometrySelector->currentIndex() - 1 );
            constrainedText->SetData( "Constrained Geometry", tempCADNode->GetID() );
        }
        toolCommand->AddDataValuePair( constrainedText );
    }

    ves::open::xml::DataValuePairPtr computerNameText( new ves::open::xml::DataValuePair() );
    computerNameText->SetData( "ComputerName", ui->m_computerName->text().toStdString() );
    toolCommand->AddDataValuePair( computerNameText );

    ves::open::xml::DataValuePairPtr computerPortText( new ves::open::xml::DataValuePair() );
    computerPortText->SetData( "ComputerPort", ui->m_portNumber->text().toStdString() );
    toolCommand->AddDataValuePair( computerPortText );

    double scaleValue = 1.0;
    //("m -> ft"), ("cm -> ft"), ("mm -> ft"), ("in -> ft")
    int choice = ui->m_unitsSelector->currentIndex();
    if( choice == 0 )
    {
        scaleValue = 3.2808399;
    }
    else if( choice == 1 )
    {
        scaleValue = 0.032808399;
    }
    else if( choice == 2 )
    {
        scaleValue = 0.0032808399;
    }
    else if( choice == 3 )
    {
        scaleValue = 0.0833333;
    }

    ves::open::xml::DataValuePairPtr simScale( new ves::open::xml::DataValuePair() );
    simScale->SetData( "Simulator Scale", scaleValue );
    toolCommand->AddDataValuePair( simScale );
    tempModel->SetInput( toolCommand );

    ves::open::xml::CommandPtr regCommand( new ves::open::xml::Command() );
    regCommand->SetCommandName( "DVST Registration Update" );
    double sipVal = 0.0;
    ves::open::xml::DataValuePairPtr sipValX( new ves::open::xml::DataValuePair() );
    sipVal = ui->m_SIPX->text().toDouble();
    sipValX->SetData( "SIP X", sipVal );
    regCommand->AddDataValuePair( sipValX );

    ves::open::xml::DataValuePairPtr sipValY( new ves::open::xml::DataValuePair() );
    sipVal = ui->m_SIPY->text().toDouble();
    sipValY->SetData( "SIP Y", sipVal );
    regCommand->AddDataValuePair( sipValY );

    ves::open::xml::DataValuePairPtr sipValZ( new ves::open::xml::DataValuePair() );
    sipVal = ui->m_SIPZ->text().toDouble();
    sipValZ->SetData( "SIP Z", sipVal );
    regCommand->AddDataValuePair( sipValZ );

    ves::open::xml::DataValuePairPtr fowardVecDVP( new ves::open::xml::DataValuePair() );
    fowardVecDVP->SetData( "Forward", ui->m_CADForwardVector->currentText().toStdString() );
    regCommand->AddDataValuePair( fowardVecDVP );

    ves::open::xml::DataValuePairPtr upVecDVP( new ves::open::xml::DataValuePair() );
    upVecDVP->SetData( "Up", ui->m_CADUpVector->currentText().toStdString() );
    regCommand->AddDataValuePair( upVecDVP );

    tempModel->SetInput( regCommand );


    ves::open::xml::CommandPtr geomCommand( new ves::open::xml::Command() );
    geomCommand->SetCommandName( "Geometry Data Map" );
    if( m_nodeList.size() > 0 )
    {
        for( size_t i = 0; i < m_geomChoiceList.size(); ++i )
        {
            ves::open::xml::DataValuePairPtr geomDVP( new ves::open::xml::DataValuePair() );
            ves::open::xml::cad::CADNodePtr tempCADNode =
                    m_nodeList.at( m_geomChoiceList.at( i )->currentIndex() );
            geomDVP->SetData( tempCADNode->GetNodeName(), tempCADNode->GetID() );
            geomCommand->AddDataValuePair( geomDVP );
        }
    }
    if( (m_geomChoiceList.size() == 0) || (m_nodeList.size() == 0) )
    {
        ves::open::xml::DataValuePairPtr geomDVP( new ves::open::xml::DataValuePair() );
        geomDVP->SetData( "No Geometry Selected", "No Geom" );
        geomCommand->AddDataValuePair( geomDVP );
    }
    tempModel->SetInput( geomCommand );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimTool_UIDialog::PopulateDialogs()
{
    ves::open::xml::model::ModelPtr tempModel = m_uiPluginBase->GetVEModel();

    ves::open::xml::CommandPtr toolCommand = tempModel->GetInput( "Tool Info" );
    std::string constrainedGeom;
    if( toolCommand )
    {
        ves::open::xml::DataValuePairPtr geomDVP = toolCommand->GetDataValuePair( "Constrained Geometry" );
        if( geomDVP )
        {
            geomDVP->GetData( constrainedGeom );
        }
    }

    ves::open::xml::cad::CADNodePtr rootNode = tempModel->GetGeometry();
    dynamicvehicletool::CADListCreator nodeListCreator( rootNode );
    m_nodeList = nodeListCreator.GetNodeList();
    std::vector< std::string > nodeListNames =
        nodeListCreator.GetNodeNameList();

    QStringList choices;
    choices.append( "None" );

    for( size_t i = 0; i < nodeListNames.size(); ++i )
    {
        choices.append( QString::fromStdString( nodeListNames.at( i ) ) );
    }

    ui->m_userConstrainedGeometrySelector->clear();
    ui->m_userConstrainedGeometrySelector->addItems( choices );
    ui->m_userConstrainedGeometrySelector->setCurrentIndex( 0 );

    size_t nodeIndex1 = 0;
    std::string selectedNode( "None" );
    for( size_t j = 0; j < m_nodeList.size(); ++j )
    {
        std::string nodeID = m_nodeList.at( j )->GetID();
        if( nodeID == constrainedGeom )
        {
            nodeIndex1 = j;
            selectedNode = nodeListNames.at( nodeIndex1 );
            break;
        }
    }
    int index = ui->m_userConstrainedGeometrySelector->findText(
                                       QString::fromStdString( selectedNode ) );
    ui->m_userConstrainedGeometrySelector->setCurrentIndex( index );

    //Setup computer info
    std::string computerName;
    if( toolCommand )
    {
        toolCommand->GetDataValuePair( "ComputerName" )->GetData( computerName );
    }
    if( computerName.empty() )
    {
        computerName = "225.0.0.37";
    }
    ui->m_computerName->setText( QString::fromStdString( computerName ) );

    std::string computerPort;
    if( toolCommand )
    {
        toolCommand->GetDataValuePair( "ComputerPort" )->GetData( computerPort );
    }
    if( computerPort.empty() )
    {
        computerPort = "12345";
    }
    ui->m_portNumber->setText( QString::fromStdString( computerPort ) );

    double scaleValue = 1.0;
    if( toolCommand )
    {
        ves::open::xml::DataValuePairPtr simScale = toolCommand->GetDataValuePair( "Simulator Scale" );
        if( simScale )
        {
            simScale->GetData( scaleValue );
        }
    }

    //("m -> ft"), ("cm -> ft"), ("mm -> ft"), ("in -> ft")
    int choice = 1;
    if( scaleValue == 3.2808399 )
    {
        choice = 0;
    }
    else if( scaleValue == 0.032808399 )
    {
        choice = 1;
    }
    else if( scaleValue == 0.0032808399 )
    {
        choice = 2;
    }
    else if( scaleValue == 0.0833333 )
    {
        choice = 3;
    }
    ui->m_unitsSelector->setCurrentIndex( choice );

    //Initialize the registration data
    //Get bird info from VR Juggler
    toolCommand = tempModel->GetInput( "DVST Registration Update" );
    if( toolCommand )
    {
        QString num;
        double sipVal = 0.0;
        ves::open::xml::DataValuePairPtr sipValDVP = toolCommand->GetDataValuePair( "SIP X" );
        sipValDVP->GetData( sipVal );
        num.setNum( sipVal );
        ui->m_SIPX->setText( num );

        sipValDVP = toolCommand->GetDataValuePair( "SIP Y" );
        sipValDVP->GetData( sipVal );
        num.setNum( sipVal );
        ui->m_SIPY->setText( num );

        sipValDVP = toolCommand->GetDataValuePair( "SIP Z" );
        sipValDVP->GetData( sipVal );
        num.setNum( sipVal );
        ui->m_SIPZ->setText( num );

        ves::open::xml::DataValuePairPtr fowardVecDVP = toolCommand->GetDataValuePair( "Forward" );
        if( fowardVecDVP )
        {
            std::string forwardVecString;
            fowardVecDVP->GetData( forwardVecString );
            int index = ui->m_CADForwardVector->findText(
                                   QString::fromStdString( forwardVecString ) );
            ui->m_CADForwardVector->setCurrentIndex( index );

            ves::open::xml::DataValuePairPtr upVecDVP = toolCommand->GetDataValuePair( "Up" );
            std::string upVecString;
            upVecDVP->GetData( upVecString );
            index = ui->m_CADUpVector->findText(
                                        QString::fromStdString( upVecString ) );
            ui->m_CADUpVector->setCurrentIndex( index );
        }
    }

    toolCommand = tempModel->GetInput( "Geometry Data Map" );
    if( !toolCommand )
    {
        return;
    }

    ///Need to clear any array choice selections
    if( m_geomChoiceList.size() > 0 )
    {
        //we already have the choices loaded up
        return;
    }

    size_t numDVPs = toolCommand->GetNumberOfDataValuePairs();
    std::string nodeName;
    for( size_t i = 0; i < numDVPs; ++i )
    {
        ves::open::xml::DataValuePairPtr geomDVP =
            toolCommand->GetDataValuePair( i );
        geomDVP->GetData( nodeName );
        if( nodeName == "No Geom" )
        {
            break;
        }

        on_m_addButton_clicked();
        size_t nodeIndex = 0;
        for( size_t j = 0; j < m_nodeList.size(); ++j )
        {
            std::string nodeID = m_nodeList.at( j )->GetID();
            if( nodeID == nodeName )
            {
                nodeIndex = j;
                break;
            }
        }
        int index = m_geomChoiceList.at( i )->findText(
                      QString::fromStdString( nodeListNames.at( nodeIndex ) ) );
        m_geomChoiceList.at( i )->setCurrentIndex( index );
    }
}
////////////////////////////////////////////////////////////////////////////////
