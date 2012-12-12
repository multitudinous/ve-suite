#pragma once

#include <QWidget>
#include <QtGui/QComboBox>
#include <QtGui/QFileDialog>

#include "DynamicVehicleSimTool.h"

#include <ves/open/xml/cad/CADNodePtr.h>
#include <ves/conductor/qt/plugin/UIPluginBase.h>

#include <switchwire/Event.h>

namespace Ui {
class DynamicVehicleSimTool_UIDialog;
}

class DynamicVehicleSimTool_UIDialog : public QWidget
{
    Q_OBJECT
    
public:
    explicit DynamicVehicleSimTool_UIDialog(QWidget *parent = 0, ves::conductor::UIPluginBase* pluginBase = 0 );
    ~DynamicVehicleSimTool_UIDialog();

    virtual bool TransferDataToWindow();
    virtual bool TransferDataFromWindow();

protected Q_SLOTS:
    // Autoconnecting slots
    void on_m_startStopButton_clicked();
    void on_m_resetButton_clicked();
    void on_m_addButton_clicked();
    void on_m_removeButton_clicked();
    void on_m_applyButton_clicked();
    void on_m_registerButton_clicked();
    void on_m_computerName_textChanged( const QString& text );
    void on_m_portNumber_textChanged( const QString& text );
    void on_m_userConstrainedGeometrySelector_currentIndexChanged( int index );
    void on_m_registrationSelector_currentIndexChanged( int index );

    // Not autoconnecting
    void onFileSelected( const QString& filePath );
    void onFileCancelled();

Q_SIGNALS:

    
private:
    void UpdateModelData();
    void PopulateDialogs();

    ves::conductor::UIPluginBase* m_uiPluginBase;
    Ui::DynamicVehicleSimTool_UIDialog *ui;

    ///List of geom cadnode names
    std::vector< QComboBox* > m_geomChoiceList;
    ///Node list
    std::vector< ves::open::xml::cad::CADNodePtr > m_nodeList;
    ///
    std::string m_fileName;

    QFileDialog* m_fileDialog;

    switchwire::Event< void( const std::string&, const std::string& ) > m_setComputerDetails;
    switchwire::Event< void( const std::string& ) > m_setSimState;
    switchwire::Event< void( const double& ) > m_setSimScale;
    switchwire::Event< void( ) > m_simReset;
    switchwire::Event< void( const std::vector< std::string >& ) > m_setGeometryMap;
    switchwire::Event< void( const std::string& ) > m_setGeometryConstraint;
    switchwire::Event< void( const std::string&,
                             const std::string&,
                             const std::vector< double >&,
                             const std::string&,
                             const std::string& ) > m_registrationUpdate;
};
