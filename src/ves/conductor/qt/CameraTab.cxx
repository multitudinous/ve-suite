#include <ves/conductor/qt/CameraTab.h>
#include <ves/conductor/qt/ui_CameraTab.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <ves/xplorer/data/CameraSettingsPropertySet.h>
#include <ves/xplorer/data/CameraModePropertySet.h>

#include <storyteller/PresentationMaker.h>

#include <Poco/Path.h>
#include <Poco/File.h>

#include <iostream>

namespace ves
{
namespace conductor
{

CameraTab::CameraTab(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::CameraTab),
    m_monotonicCount( 0 ),
    m_notifyCameraChange( true ),
    m_cameraWindowEnabled( false ),
    m_logger( Poco::Logger::get( "conductor.CameraTab" ) ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    ui->setupUi(this);

    m_overallSet = propertystore::PropertySetPtr( new ves::xplorer::data::CameraModePropertySet );
    //Set by default in the propertyset
    //m_overallSet->LoadByKey( "uuid", std::string( "00000000-0101-1010-1111-000011110000" ) );
    //if( m_overallSet )
    {
        m_overallSet->EnableLiveProperties( true );
        m_overallSet->SetPropertyValue( "DisableCameraTools", false );
        m_overallSet->SetPropertyValue( "CameraWindow", false );

        ui->m_overallSettings->ParsePropertySet( m_overallSet );
        ui->m_overallSettings->show();
    }
    ui->m_showHideSettingsButton->toggle();
    ui->m_pictureModeToggler->hide();

    using namespace ves::xplorer;

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_addCameraSignal ),
        "CameraTab.AddCamera" );
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_removeCameraSignal ),
        "CameraTab.RemoveCamera" );
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_selectCameraSignal ),
        "CameraTab.SelectCamera" );
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_saveCameraImageSignal ),
        "CameraTab.SaveCameraImage" );
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_saveAllCameraImagesSignal ),
        "CameraTab.SaveAllCameraImages" );
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_flythroughSignal ),
        "CameraTab.BeginFlythrough" );
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_endFlythroughSignal ),
        "CameraTab.EndFlythrough" );
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_loopFlythroughSignal ),
        "CameraTab.LoopFlythrough" );

    CONNECTSIGNALS_1( "%PictureModeOn",
                    void( bool ),
                    &ves::conductor::CameraTab::PictureModeOn,
                    m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
CameraTab::~CameraTab()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::PictureModeOn( bool flag )
{
    // There is a hidden checkbox on the ui form that is set up in the .ui file
    // to toggle a bunch of stuff on and off in response to this flag. Look to
    // CameraTab.ui to change which widgets get toggled.
    ui->m_pictureModeToggler->setChecked( flag );
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_cameraListWidget_currentItemChanged(
        QListWidgetItem* current, QListWidgetItem* )
{
    if( current )
    {
        std::string uuid = current->data( Qt::UserRole ).toString().toStdString();
        if( m_notifyCameraChange )
        {
            m_selectCameraSignal.signal( uuid );
        }
        m_perCameraSet = propertystore::PropertySetPtr( new ves::xplorer::data::CameraSettingsPropertySet );
        m_perCameraSet->SetUUID( uuid );
        m_perCameraSet->EnableLiveProperties( true );
        m_perCameraSet->Load();
    }
    else
    {
        m_perCameraSet = propertystore::PropertySetPtr();
    }

    ui->m_perCameraSettings->ParsePropertySet( m_perCameraSet );
    ui->m_perCameraSettings->show();
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_addCameraButton_clicked()
{
    QString name("Camera");
    QString num;
    num.setNum( m_monotonicCount );
    name += num;
    m_monotonicCount++;

    QListWidgetItem* cameraItem = new QListWidgetItem( name );
    propertystore::PropertySetPtr tempSet;
    tempSet = propertystore::PropertySetPtr( new ves::xplorer::data::CameraSettingsPropertySet );
    tempSet->SetPropertyValue( "NameTag", name.toStdString() );
    std::string uuid = tempSet->GetUUIDAsString();
    tempSet->Save();
    cameraItem->setData( Qt::UserRole, QString::fromStdString( uuid ) );

    m_addCameraSignal.signal( uuid, name.toStdString() );

    ui->m_cameraListWidget->addItem( cameraItem );
    ui->m_cameraListWidget->setCurrentItem( cameraItem );
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_removeCameraButton_clicked()
{
    QListWidgetItem* item = ui->m_cameraListWidget->currentItem();
    if( !item )
    {
        return;
    }
    // TODO: pull user data from item to get UUID and remove this entry from store.
    propertystore::PropertySetPtr tempSet;
    tempSet = propertystore::PropertySetPtr( new ves::xplorer::data::CameraSettingsPropertySet );
    std::string uuid( item->data( Qt::UserRole ).toString().toStdString() );
    tempSet->SetUUID( uuid );
    tempSet->Remove();
    delete item;
    m_removeCameraSignal.signal( uuid );
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_cameraUpButton_clicked()
{
    int currentRow = ui->m_cameraListWidget->currentRow();

    if( (currentRow == 0) || (currentRow == -1) )
    {
        return;
    }

    // Suppress camera selection signal during this process
    m_notifyCameraChange = false;

    QListWidgetItem* item = ui->m_cameraListWidget->takeItem( currentRow );
    ui->m_cameraListWidget->insertItem( currentRow - 1, item );
    ui->m_cameraListWidget->setCurrentRow( currentRow - 1 );

    m_notifyCameraChange = true;
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_cameraDownButton_clicked()
{
    int currentRow = ui->m_cameraListWidget->currentRow();

    if( currentRow == ( ui->m_cameraListWidget->count() - 1 ) )
    {
        return;
    }

    // Suppress camera selection signal during this process
    m_notifyCameraChange = false;

    QListWidgetItem* item = ui->m_cameraListWidget->takeItem( currentRow );
    ui->m_cameraListWidget->insertItem( currentRow + 1, item );
    ui->m_cameraListWidget->setCurrentRow( currentRow + 1 );

    m_notifyCameraChange = true;
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_snapshotButton_clicked()
{
    //If we are in non-picture mode
    if( !ui->m_pictureModeToggler->isChecked() )
    {
        //Grab the active camera if there is one.
        QListWidgetItem* item = ui->m_cameraListWidget->currentItem();
        if( item )
        {
            // Empty path forces default to value of CameraImageSavePath in
            // CameraModePropertySet
            m_saveCameraImageSignal.signal( item->data( Qt::UserRole ).toString().toStdString(), "" );
        }
    }
    else
    {
        // Empty path forces default to value of CameraImageSavePath in
        // CameraModePropertySet
        m_saveCameraImageSignal.signal( "PICTURE_MODE", "" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_allSnapshotButton_clicked()
{
    if( !m_overallSet )
    {
        return;
    }

    // Empty path forces default to value of CameraImageSavePath in
    // CameraModePropertySet
    m_saveAllCameraImagesSignal.signal( "" );
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_presentationButton_clicked()
{
    if( !m_overallSet )
    {
        return;
    }

    // Build up a temporary directory based on CameraImageSavePath value of
    // CameraModePropertySet, with ves_tmp appended.
    std::string savePathBase = boost::any_cast<std::string>( m_overallSet->GetPropertyValue( "CameraImageSavePath" ) );

    if( !ui->m_pictureModeToggler->isChecked() )
    {
        Poco::Path imagePath( savePathBase );
        imagePath.setFileName( "" );
        imagePath.makeAbsolute();
        imagePath.pushDirectory( "ves_tmp" );
        Poco::File tmpFile( imagePath );
        if( tmpFile.exists() )
        {
            tmpFile.remove( true );
        }
        tmpFile.createDirectory();

        m_presentationImageDir = imagePath.toString();

        m_saveAllCameraImagesSignal.signal( m_presentationImageDir );
        // Connect to the "done" signal for this process so we will know when the
        // images have been written. This doesn't happen synchronously, so we can't
        // assume the images exist until we get this signal.
        CONNECTSIGNALS_0( "CameraManager.CameraImagesSaved",
                        void( ),
                        &ves::conductor::CameraTab::MakePresentation,
                        m_presentationConnections, any_SignalType, normal_Priority );
    }
    else
    {
        m_presentationImageDir = savePathBase;

        MakePresentation();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::MakePresentation()
{
    if( !ui->m_pictureModeToggler->isChecked() )
    {
        if( ui->m_cameraListWidget->count() == 0 )
        {
            return;
        }
    }
    
    m_presentationConnections.DropConnections();

    Poco::Path presPath( m_presentationImageDir );
    presPath.makeAbsolute();
    m_presentationImageDir = presPath.toString();

    storyteller::PresentationMaker pm;
    pm.AddDirectory( m_presentationImageDir, false );
    
    if( !ui->m_pictureModeToggler->isChecked() )
    {
        presPath.popDirectory();
    }
    presPath.setFileName( "presentation.odp" );

    pm.MakePresentation( presPath.toString() );

    // Remove the temporary image directory
    if( !ui->m_pictureModeToggler->isChecked() )
    {
        Poco::Path remPath( m_presentationImageDir );
        Poco::File tmpFile( remPath );
        if( tmpFile.exists() )
        {
            tmpFile.remove( true );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_flythroughButton_clicked()
{
    if( ui->m_cameraListWidget->count() == 0 )
    {
        return;
    }

    for( int index = 0; index < ui->m_cameraListWidget->count(); ++index )
    {
        QListWidgetItem* item = ui->m_cameraListWidget->item( index );
        std::string uuid = item->data( Qt::UserRole ).toString().toStdString();
        m_flythroughList.push_back( uuid );
    }

    // Hide all camera and frustum geometry
    if( m_overallSet )
    {
        m_overallSet->SetPropertyValue( "DisableCameraTools", true );

        // Hide the camera window, but remember what its setting was beforehand
        m_cameraWindowEnabled = boost::any_cast< bool >( m_overallSet->GetPropertyValue( "CameraWindow" ) );
        m_overallSet->SetPropertyValue( "CameraWindow", false );
    }

    // Request notification when flythrough is done so we can undo hiding camera
    // geometry and such
    CONNECTSIGNALS_0( "%FlythroughEnd",
                    void( ),
                    &ves::conductor::CameraTab::FlythroughHasEnded,
                    m_flythroughConnections, any_SignalType, normal_Priority );

    m_flythroughSignal.signal( m_flythroughList );
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::on_m_loopingFlythroughButton_toggled( bool flag )
{
    if( flag )
    {
        m_loopFlythroughSignal.signal( true );
        on_m_flythroughButton_clicked();
    }
    else
    {
        m_loopFlythroughSignal.signal( false );
        m_endFlythroughSignal.signal();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraTab::FlythroughHasEnded()
{
    m_flythroughList.clear();

    m_flythroughConnections.DropConnections();

    if( m_overallSet )
    {
        m_overallSet->SetPropertyValue( "DisableCameraTools", false );
        m_overallSet->SetPropertyValue( "CameraWindow",
                                        m_cameraWindowEnabled );
    }
}
////////////////////////////////////////////////////////////////////////////////
}} // ves::conductor
