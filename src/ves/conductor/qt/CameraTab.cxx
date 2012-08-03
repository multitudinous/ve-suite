#include <ves/conductor/qt/CameraTab.h>
#include <ves/conductor/qt/ui_CameraTab.h>

#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>
#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/data/CameraSettingsPropertySet.h>
#include <ves/xplorer/data/CameraModePropertySet.h>

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
    m_logger( Poco::Logger::get( "conductor.CameraTab" ) ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    ui->setupUi(this);
    m_perCameraSettingsBrowser = new PropertyBrowser( this );
    m_overallSettingsBrowser = new PropertyBrowser( this );

    m_overallSet = ves::xplorer::data::PropertySetPtr( new ves::xplorer::data::CameraModePropertySet );
    if( m_overallSet )
    {
        m_overallSet->EnableLiveProperties( true );
        m_overallSet->SetPropertyValue( "DisableCameraTools", false );
        m_overallSet->SetPropertyValue( "CameraWindow", true );
        m_overallSettingsBrowser->ParsePropertySet( m_overallSet );
        m_overallSettingsBrowser->RefreshAll();

        ui->m_overallSettings->setPropertyBrowser( m_overallSettingsBrowser );
        ui->m_overallSettings->RefreshContents();
        ui->m_overallSettings->show();
    }
    ui->m_showHideSettingsButton->toggle();
    ui->m_pictureModeToggler->hide();

    using namespace ves::xplorer;

    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::TwoStringSignal_type >( &m_addCameraSignal ),
        "CameraTab.AddCamera" );
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::StringSignal_type >( &m_removeCameraSignal ),
        "CameraTab.RemoveCamera" );
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::StringSignal_type >( &m_selectCameraSignal ),
        "CameraTab.SelectCamera" );
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::StringSignal_type >( &m_saveCameraImageSignal ),
        "CameraTab.SaveCameraImage" );
    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ves::util::VoidSignal_type >( &m_saveAllCameraImagesSignal ),
        "CameraTab.SaveAllCameraImages" );

    CONNECTSIGNALS_1( "%PictureModeOn",
                    void( bool ),
                    &ves::conductor::CameraTab::PictureModeOn,
                    m_connections, any_SignalType, normal_Priority );
}

CameraTab::~CameraTab()
{
    delete ui;
    delete m_perCameraSettingsBrowser;
    delete m_overallSettingsBrowser;
}

void CameraTab::PictureModeOn( bool flag )
{
    // There is a hidden checkbox on the ui form that is set up in the .ui file
    // to toggle a bunch of stuff on and off in response to this flag. Look to
    // CameraTab.ui to change which widgets get toggled.
    ui->m_pictureModeToggler->setChecked( flag );
}

void CameraTab::on_m_cameraListWidget_currentItemChanged(
        QListWidgetItem* current, QListWidgetItem* )
{
    if( current )
    {
        std::string uuid = current->data( Qt::UserRole ).toString().toStdString();
        if( m_notifyCameraChange )
        {
            m_selectCameraSignal( uuid );
        }
        m_perCameraSet = ves::xplorer::data::PropertySetPtr( new ves::xplorer::data::CameraSettingsPropertySet );
        m_perCameraSet->SetUUID( uuid );
        m_perCameraSet->EnableLiveProperties( true );
        m_perCameraSet->LoadFromDatabase();
    }
    else
    {
        m_perCameraSet = ves::xplorer::data::PropertySetPtr();
    }
    m_perCameraSettingsBrowser->ParsePropertySet( m_perCameraSet );
    m_perCameraSettingsBrowser->RefreshAll();

    ui->m_perCameraSettings->setPropertyBrowser( m_perCameraSettingsBrowser );
    ui->m_perCameraSettings->RefreshContents();
    ui->m_perCameraSettings->show();
}

void CameraTab::on_m_addCameraButton_clicked()
{
    QString name("Camera");
    QString num;
    num.setNum( m_monotonicCount );
    name += num;
    m_monotonicCount++;

    QListWidgetItem* cameraItem = new QListWidgetItem( name );
    ves::xplorer::data::PropertySetPtr tempSet;
    tempSet = ves::xplorer::data::PropertySetPtr( new ves::xplorer::data::CameraSettingsPropertySet );
    tempSet->SetPropertyValue( "NameTag", name.toStdString() );
    std::string uuid = tempSet->GetUUIDAsString();
    tempSet->WriteToDatabase();
    cameraItem->setData( Qt::UserRole, QString::fromStdString( uuid ) );

    m_addCameraSignal( uuid, name.toStdString() );

    ui->m_cameraListWidget->addItem( cameraItem );
    ui->m_cameraListWidget->setCurrentItem( cameraItem );
}

void CameraTab::on_m_removeCameraButton_clicked()
{
    QListWidgetItem* item = ui->m_cameraListWidget->currentItem();
    if( !item )
    {
        return;
    }
    // TODO: pull user data from item to get UUID and remove this entry from store.
    ves::xplorer::data::PropertySetPtr tempSet;
    tempSet = ves::xplorer::data::PropertySetPtr( new ves::xplorer::data::CameraSettingsPropertySet );
    std::string uuid( item->data( Qt::UserRole ).toString().toStdString() );
    tempSet->SetUUID( uuid );
    tempSet->DeleteFromDatabase();
    delete item;
    m_removeCameraSignal( uuid );
}

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

void CameraTab::on_m_snapshotButton_clicked()
{
    if( !ui->m_pictureModeToggler->isChecked() )
    {
        QListWidgetItem* item = ui->m_cameraListWidget->currentItem();
        if( item )
        {
            m_saveCameraImageSignal( item->data( Qt::UserRole ).toString().toStdString() );
        }
    }
    else
    {
        m_saveCameraImageSignal( "PICTURE_MODE" );
    }
}

void CameraTab::on_m_allSnapshotButton_clicked()
{
    m_saveAllCameraImagesSignal();
}

void CameraTab::on_m_presentationButton_clicked()
{

}

void CameraTab::on_m_flythroughButton_clicked()
{

}

}} // ves::conductor
