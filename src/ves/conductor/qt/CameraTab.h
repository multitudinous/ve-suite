#pragma once

#include <QtGui/QWidget>

#include <ves/xplorer/data/PropertySetPtr.h>

#include <ves/xplorer/Logging.h>
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>

#include <QtGui/QListWidgetItem>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

namespace Ui {
class CameraTab;
}

namespace ves
{
namespace conductor
{
class PropertyBrowser;

class CameraTab : public QWidget
{
    Q_OBJECT
    
public:
    explicit CameraTab(QWidget *parent = 0);
    ~CameraTab();

private Q_SLOTS:
    void on_m_addCameraButton_clicked();
    void on_m_removeCameraButton_clicked();
    void on_m_cameraUpButton_clicked();
    void on_m_cameraDownButton_clicked();
    void on_m_snapshotButton_clicked();
    void on_m_allSnapshotButton_clicked();
    void on_m_presentationButton_clicked();
    void on_m_flythroughButton_clicked();
    void on_m_cameraListWidget_currentItemChanged( QListWidgetItem* current,
                                                   QListWidgetItem*  );
    
private:
    void PictureModeOn( bool flag );

    Ui::CameraTab *ui;

    PropertyBrowser* m_overallSettingsBrowser;
    PropertyBrowser* m_perCameraSettingsBrowser;
    ves::xplorer::data::PropertySetPtr m_overallSet;
    ves::xplorer::data::PropertySetPtr m_perCameraSet;

    int m_monotonicCount;

    ///When true, we emit signal to notify of active camera selection change.
    ///List order changes cause two selections in quick succession, neither of
    ///which should be treated as an actual change in the active camera. This
    ///flag lets us suppress those changes.
    bool m_notifyCameraChange;

    ///Logger reference
    Poco::Logger& m_logger;

    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;

    ///Required to connect to EventManagered signals
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;

    ves::util::TwoStringSignal_type m_addCameraSignal;
    ves::util::StringSignal_type m_selectCameraSignal;
    ves::util::StringSignal_type m_saveCameraImageSignal;
    ves::util::VoidSignal_type m_saveAllCameraImagesSignal;
    ves::util::StringSignal_type m_removeCameraSignal;

};

}} // ves::conductor
