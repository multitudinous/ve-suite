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
#define QT_NO_KEYWORDS
#include "MainWindow.h"

#include <QtGui/QPaintEvent>
#include <QtGui/QToolButton>
#include <QtCore/QDir>
#include <QtCore/QSettings>

#include <ves/conductor/qt/ExtendedTabWidget.h>

#include <ves/conductor/qt/ui_MainWindow.h>

#include <ves/conductor/qt/Visualization.h>
#include <ves/conductor/qt/Constraints.h>
#include <ves/conductor/qt/NetworkLoader.h>
#include <ves/conductor/qt/CADFileLoader.h>
#include <ves/conductor/qt/IconStack.h>
#include <ves/conductor/qt/TreeTab.h>
#include <ves/conductor/qt/PreferencesTab.h>
#include <ves/conductor/qt/plugin/PluginSelectionTab.h>
//#include <ves/conductor/qt/VisFeatureManager.h>
#include <ves/conductor/qt/CameraTab.h>
#include <ves/conductor/qt/XMLDataBufferEngine.h>
#include <ves/conductor/qt/extendedWidgets/ExtendedToolBar.h>
#include <ves/conductor/qt/UITabs.h>
#include <ves/conductor/qt/RecentFiles.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/model/Network.h>

// Req'd for dataset loads
#include <ves/open/xml/ParameterBlock.h>
#include <ves/xplorer/util/fileIO.h>
#include <ves/xplorer/util/cfdVTKFileHandler.h>
#include <ves/open/xml/OneDStringArray.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
#include <ves/xplorer/eventmanager/EventFactory.h>
//#include <switchwire/Event.h>

#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/data/CADPropertySet.h>

#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#ifdef MINERVA_GIS_SUPPORT
# include <ves/xplorer/minerva/MinervaManager.h>
# include <ves/conductor/qt/minerva/StackedWidget.h>

#include <Minerva/Core/TileEngine/Body.h>
#endif

//#define BOOST_FILESYSTEM_VERSION 3
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>

#include <vrj/Kernel/Kernel.h>

#include <vpr/Perf/ProfileManager.h>

#include <iostream>

using namespace ves::xplorer;
using namespace ves::conductor;

///The Q_DECLARE_METATYPE maco allows us to use non-Qt types in
///queued connections in Qt-signals
Q_DECLARE_METATYPE( std::string )
////////////////////////////////////////////////////////////////////////////////
MainWindow::MainWindow( QWidget* parent, const std::string& features ) :
    QMainWindow( parent ),
    ui( new Ui::MainWindow ),
    mFileDialog( 0 ),
    mFileOpsStack( 0 ),
    m_physicsMenuStack( 0 ),
    mScenegraphTreeTab( 0 ),
    mActiveTab( "" ),
    mVisualizationTab( 0 ),
    mMinervaStackedWidget( 0 ),
    m_preferencesTab( 0 ),
    m_pluginsTab( 0 ),
    m_constraintsTab( 0 ),
    m_recentTab( 0 ),
    m_cameraTab( 0 ),
    m_logger( Poco::Logger::get( "conductor.MainWindow" ) ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    setMouseTracking( true );
    ui->setupUi( this );
    //m_messageBox = new QMessageBox(this);
    ui->menuBar->setContextMenuPolicy( Qt::PreventContextMenu );

    QToolBar* toolbar = new ExtendedToolBar( this );
    toolbar->setContextMenuPolicy( Qt::PreventContextMenu );
    toolbar->setMouseTracking( true );
    toolbar->setIconSize( QSize( 32, 32 ) );
    toolbar->setMovable( false );
    this->addToolBar( toolbar );

    QString tempFeatures = QString::fromStdString( features );
    // Set up the default options
    // This is where to add anything that should be in the default features
    if( tempFeatures == "VESDefault" )
    {
        tempFeatures.clear();
        tempFeatures.append( "Physics,Manipulator,Navigation,Plugins," );
        tempFeatures.append( "Constraints,Visualization,Tree,GIS" );
    }
    m_displayFeatures = tempFeatures.split( "," );

    ///The file menu stack
    {
        toolbar->addAction( ui->actionFile );

        mFileOpsStack = new IconStack( toolbar->
                                       widgetForAction( ui->actionFile ), this );
        mFileOpsStack->SetExtendedToolBarParent( toolbar );
        mFileOpsStack->AddAction( ui->actionNew );
        mFileOpsStack->AddAction( ui->actionOpen );
        mFileOpsStack->AddAction( ui->actionSave );
        mFileOpsStack->setObjectName( "mFileOpsStack" );
    }

    ///The physics menu stack
    if( m_displayFeatures.contains( "Physics" ) )
    {
        toolbar->addAction( ui->actionPhysicsStack );

        m_physicsMenuStack = new IconStack( toolbar->
                                            widgetForAction( ui->actionPhysicsStack ), this );
        m_physicsMenuStack->SetExtendedToolBarParent( toolbar );
        m_physicsMenuStack->AddAction( ui->actionPlayPhysics );
        m_physicsMenuStack->AddAction( ui->actionPausePhysics );
        m_physicsMenuStack->AddAction( ui->actionResetPhysics );
        m_physicsMenuStack->AddAction( ui->actionStepPhysics );
        m_physicsMenuStack->setObjectName( "m_physicsMenuStack" );
    }

    ///The manipulator menu stack
    if( m_displayFeatures.contains( "Manipulator" ) )
    {
        toolbar->addAction( ui->actionManipulatorStack );

        m_manipulatorMenuStack = new IconStack( toolbar->
                                                widgetForAction( ui->actionManipulatorStack ), this );
        m_manipulatorMenuStack->SetExtendedToolBarParent( toolbar );
        m_manipulatorMenuStack->AddAction( ui->actionEnableManipulator );
        m_manipulatorMenuStack->AddAction( ui->actionScaleManipulator );
        m_manipulatorMenuStack->AddAction( ui->actionTranslateManipulator );
        m_manipulatorMenuStack->AddAction( ui->actionRotateManipulator );
        m_manipulatorMenuStack->AddAction( ui->actionComboManipulator );
        m_manipulatorMenuStack->setObjectName( "m_manipulatorMenuStack" );
        //ui->actionManipulatorStack

        ui->actionScaleManipulator->setEnabled( false );
        ui->actionTranslateManipulator->setEnabled( false );
        ui->actionRotateManipulator->setEnabled( false );
        ui->actionComboManipulator->setEnabled( false );
    }

    ///The nav menu stack
    if( m_displayFeatures.contains( "Navigation" ) )
    {
        toolbar->addAction( ui->actionNavigationStack );

        m_navMenuStack = new IconStack( toolbar->
                                        widgetForAction( ui->actionNavigationStack ), this );
        m_navMenuStack->SetExtendedToolBarParent( toolbar );
        m_navMenuStack->AddAction( ui->actionSmallJump );
        m_navMenuStack->AddAction( ui->actionMediumJump );
        m_navMenuStack->AddAction( ui->actionLargeJump );
        m_navMenuStack->AddAction( ui->actionBoundingBoxJump );
        m_navMenuStack->AddAction( ui->actionWorldNavigation );
        m_navMenuStack->AddAction( ui->actionObjectNavigation );
        m_navMenuStack->AddAction( ui->actionCharacterNavigation );
        m_navMenuStack->AddAction( ui->actionCharacterFlyMode );
        m_navMenuStack->setObjectName( "m_navMenustack" );

        switchwire::EventManager::instance()->RegisterSignal(
            ( &m_jumpSignal ),
            "MainWindow.JumpSignal" );

        switchwire::EventManager::instance()->RegisterSignal(
            ( &m_characterEnable ),
            "MainWindow.CharacterUpdate" );
    }

    // The view stack
    {
        toolbar->addAction( ui->actionViewStack );
        m_viewMenuStack = new IconStack( toolbar->
                                 widgetForAction( ui->actionViewStack ), this );
        m_viewMenuStack->SetExtendedToolBarParent( toolbar );
        m_viewMenuStack->AddAction( ui->actionShowPreferencesTab );
        if( m_displayFeatures.contains( "Plugins" ) )
        {
            m_viewMenuStack->AddAction( ui->actionShowPluginsTab );
        }
        m_viewMenuStack->AddAction( ui->actionRecent );
        if( m_displayFeatures.contains( "Constraints" ) )
        {
            m_viewMenuStack->AddAction( ui->actionConstraints );
        }
        //m_viewMenuStack->AddAction( ui->actionShowTestPlot );
        m_viewMenuStack->AddAction( ui->actionShowCameraTab );

        m_viewMenuStack->setObjectName( "m_viewMenuStack" );

    }

    // The GIS stack
#ifdef MINERVA_GIS_SUPPORT

    {
        if( m_displayFeatures.contains( "GIS" ) )
        {
            toolbar->addAction( ui->actionGISStack );
            m_gisMenuStack = new IconStack( toolbar->
                                  widgetForAction( ui->actionGISStack ), this );
            m_gisMenuStack->SetExtendedToolBarParent( toolbar );
            m_gisMenuStack->AddAction( ui->actionAdd_Planet );
            m_gisMenuStack->AddAction( ui->actionRemove_Planet );
            m_gisMenuStack->AddAction( ui->actionConfigure_Layers );
            m_gisMenuStack->setObjectName( "m_gisMenuStack" );
        }
    }
#endif

    // Make sure there is no statusbar on this widget.
    //setStatusBar(0);

    // Create set of default dialogs that can be added as tabs
    if( m_displayFeatures.contains( "Visualization" ) )
    {
        mVisualizationTab = new ves::conductor::Visualization( 0 );
    }
    if( m_displayFeatures.contains( "Tree" ) )
    {
        mScenegraphTreeTab = new ves::conductor::TreeTab( 0 );
    }
    m_preferencesTab = new ves::conductor::PreferencesTab( 0 );
    if( m_displayFeatures.contains( "Plugins" ) )
    {
        m_pluginsTab = new ves::conductor::PluginSelectionTab( this, 0 );
    }
    if( m_displayFeatures.contains( "Constraints" ) )
    {
        m_constraintsTab = new ves::conductor::Constraints( 0 );
    }

    //TODO: guard this with displayFeatures
    m_cameraTab = new ves::conductor::CameraTab( 0 );

    this->on_actionRecent_triggered();

    ves::conductor::UITabs::instance()->SetChild( this );

    // Connect queued signals for all slots connected via EventManager to ensure
    // that widgets can be altered during slot execution. All EventManager slots
    // should emit one of the Qt signals listed below if they alter any widget in
    // any way.
    qRegisterMetaType<std::string>();
    QObject::connect( this, SIGNAL( ActiveModelChanged( std::string ) ),
                      this, SLOT( QueuedOnActiveModelChanged( std::string ) ),
                      Qt::QueuedConnection );

    QObject::connect( this, SIGNAL( RemoveNotifierQSignal( std::string ) ),
                      this, SLOT( QueuedRemoveNotifier( std::string ) ),
                      Qt::QueuedConnection );

    connect( this, SIGNAL( UseAsSurfaceDataQSignal( std::string, bool ) ),
             this, SLOT( UseAsSurfaceDataQueued( std::string, bool ) ),
             Qt::QueuedConnection );

    // Connect to the ActiveModelChangedSignal so we can show the correct
    // tabs when the model changes
    CONNECTSIGNAL_1( "ModelHandler.ActiveModelChangedSignal",
                     void ( const std::string& ),
                     &MainWindow::OnActiveModelChanged,
                     mConnections, normal_Priority );
    // Connect to NodeAdded signal, which is sent out whenever CAD is added to the
    // scenegraph
    CONNECTSIGNALS_1( "%NodeAdded",
                      void( const std::string& ),
                      &MainWindow::RemoveNotifier,
                      mConnections, any_SignalType, normal_Priority );
    // Connect to VesFileLoaded signal, which is sent out when loading of a .ves
    // finishes
    CONNECTSIGNAL_1( "VesFileLoaded",
                     void ( const std::string& ),
                     &MainWindow::RemoveNotifier,
                     mConnections, normal_Priority );
    // Connect to DatafileLoaded signal, which is sent out when loading of a
    // dataset finishes.
    CONNECTSIGNAL_1( "AddVTKDataSetEventHandler.DatafileLoaded",
                     void ( const std::string& ),
                     &MainWindow::RemoveNotifier,
                     mConnections, normal_Priority );

    CONNECTSIGNALS_2( "%UseAsSurfaceData%", void( const std::string&, bool ),
                      &MainWindow::UseAsSurfaceData,
                      mConnections, any_SignalType, normal_Priority );


    m_GeometryExtensions.push_back( "osg" );
    m_GeometryExtensions.push_back( "ive" );
    m_GeometryExtensions.push_back( "stl" );
    m_GeometryExtensions.push_back( "wrl" );
    m_GeometryExtensions.push_back( "iv" );
    m_GeometryExtensions.push_back( "obj" );
    m_GeometryExtensions.push_back( "pfb" );
    m_GeometryExtensions.push_back( "flt" );
    m_GeometryExtensions.push_back( "dxf" );
    m_GeometryExtensions.push_back( "3ds" );
    m_GeometryExtensions.push_back( "png" );

    m_DataExtensions.push_back( "vtk" );
    m_DataExtensions.push_back( "vtu" );
    m_DataExtensions.push_back( "vts" );
    m_DataExtensions.push_back( "vti" );
    m_DataExtensions.push_back( "vtm" );
    m_DataExtensions.push_back( "vtp" );
    m_DataExtensions.push_back( "vtr" );
    m_DataExtensions.push_back( "param" );
    m_DataExtensions.push_back( "ens" );
    m_DataExtensions.push_back( "case" );
    m_DataExtensions.push_back( "mfix" );
    m_DataExtensions.push_back( "cas" );
    m_DataExtensions.push_back( "avs" );
    m_DataExtensions.push_back( "dcm" );
}
////////////////////////////////////////////////////////////////////////////////
MainWindow::~MainWindow()
{
    delete ui;

#ifdef MINERVA_GIS_SUPPORT
    delete mMinervaStackedWidget;

    ves::xplorer::minerva::MinervaManager::instance()->Clear();
#endif
}
////////////////////////////////////////////////////////////////////////////////
/*void MainWindow::mouseMoveEvent( QMouseEvent* )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::mousePressEvent( QMouseEvent* )
{
    ;
}*/
////////////////////////////////////////////////////////////////////////////////
void MainWindow::changeEvent( QEvent* e )
{
    QMainWindow::changeEvent( e );
    switch( e->type() )
    {
    case QEvent::LanguageChange:
    {
        ui->retranslateUi( this );
        break;
    }
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
int MainWindow::AddTab( QWidget* widget, const std::string& tabLabel, bool deleteOnClose )
{
    int index = ui->tabWidget->addTab( widget,
                                       QString::fromStdString( tabLabel ) );
    mTabbedWidgets[ tabLabel ] = std::pair< QWidget*, bool>
                                 ( widget, deleteOnClose );
    return index;
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::RemoveTab( QWidget* widget )
{
    if( !widget )
    {
        return;
    }

    // Remove the visual tab
    ui->tabWidget->removeTab( ui->tabWidget->indexOf( widget ) );

    // Remove this tab from mTabbedWidgets map
    std::map< std::string, std::pair< QWidget*, bool > >::iterator iter;
    for( iter = mTabbedWidgets.begin(); iter != mTabbedWidgets.end(); iter++ )
    {
        if( iter->second.first == widget )
        {
            // If tab was setup with deleteOnClose true...
            if( iter->second.second )
            {
                delete iter->second.first;
            }
            mTabbedWidgets.erase( iter );
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::RemoveTab( const std::string& tabLabel )
{
    std::map< std::string, std::pair< QWidget*, bool > >::iterator marked = mTabbedWidgets.find( tabLabel );
    if( marked != mTabbedWidgets.end() )
    {
        RemoveTab( marked->second.first );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::RemoveAllTabs()
{
    ui->tabWidget->clear();

    // Handle any tabs that require deleteOnClose behavior
    std::map< std::string, std::pair< QWidget*, bool > >::iterator iter;
    for( iter = mTabbedWidgets.begin(); iter != mTabbedWidgets.end(); iter++ )
    {
        if( iter->second.second )
        {
            delete iter->second.first;
        }
    }

    mTabbedWidgets.clear();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::ActivateTab( QWidget* widget )
{
    // Get the widget's index and call the (int) overloaded version
    ActivateTab( ui->tabWidget->indexOf( widget ) );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::ActivateTab( const std::string& tabLabel )
{
    std::map< std::string, std::pair< QWidget*, bool > >::iterator iter =
        mTabbedWidgets.find( tabLabel );
    if( iter != mTabbedWidgets.end() )
    {
        // Get the associated widget and call the (QWidget*) overloaded version
        ActivateTab( iter->second.first );
    }
}
////////////////////////////////////////////////////////////////////////////////
/// This version is the final destination of the other overloaded versions.
/// All base functionality should happen here.
void MainWindow::ActivateTab( int index )
{
    if( index < ui->tabWidget->count() )
    {
        ui->tabWidget->setCurrentIndex( index );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_tabWidget_currentChanged( int index )
{
    // Store off the name of this tab so this tab can be reactivated when
    // tabs get added/moved/removed. We use the name rather than the index
    // because the index could change. We use the name rather than the widget
    // because in our scheme, the widget on a tab page might be replaced by
    // a different widget, but given the same name. The name indicates
    // tab functionality for us, so the name is treated as primary.
    mActiveTab = ui->tabWidget->tabText( index ).toStdString();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionFile_triggered()
{
    if( mFileOpsStack->isVisible() )
    {
        mFileOpsStack->hide();
    }
    else
    {
        mFileOpsStack->Show();
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionPhysicsStack_triggered()
{
    if( m_physicsMenuStack->isVisible() )
    {
        m_physicsMenuStack->hide();
    }
    else
    {
        m_physicsMenuStack->Show();
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionQuit_triggered()
{
    VPR_PROFILE_RESULTS();
    // Stopping kernel
    vrj::Kernel::instance()->stop();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionOpen_triggered()
{
    onRecentFileRejected();
    // Don't allow multiple file dialogs to be opened.
    if( mFileDialog )
    {
        mFileDialog->close();
        mFileDialog = 0;
        //return;
    }

    mFileDialog = new QFileDialog( 0 );
    // Ensure that we use Qt's internal file dialog class since native file
    // dialogs cannot be embedded in a QTabWidget
    mFileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    // Make mFileDialog manage its own lifetime and memory
    mFileDialog->setAttribute( Qt::WA_DeleteOnClose );
    mFileDialog->setFileMode( QFileDialog::ExistingFiles );
    // Tell the dialog to open in the cwd
    mFileDialog->setDirectory( QDir::current() );

    std::string AllGeometryExtensions;
    std::vector< std::string >::iterator iter = m_GeometryExtensions.begin();
    while( iter != m_GeometryExtensions.end() )
    {
        AllGeometryExtensions.append( "*." );
        AllGeometryExtensions.append( *iter );
        AllGeometryExtensions.append( " " );
        ++iter;
    }

    std::string AllDataExtensions;
    iter = m_DataExtensions.begin();
    while( iter != m_DataExtensions.end() )
    {
        AllDataExtensions.append( "*." );
        AllDataExtensions.append( *iter );
        AllDataExtensions.append( " " );
        ++iter;
    }

    std::string AllFilters = "All Supported Files (*.ves ";
    AllFilters.append( AllGeometryExtensions );
    AllFilters.append( AllDataExtensions );
    AllFilters.append( ")" );

    std::string GeometryFilters = "Geometry Files (";
    GeometryFilters.append( AllGeometryExtensions );
    GeometryFilters.append( ")" );

    std::string DataFilters = "Data Files (";
    DataFilters.append( AllDataExtensions );
    DataFilters.append( ")" );

    QStringList filters;
    filters << AllFilters.c_str()
            << "VES Files (*.ves)"
            << GeometryFilters.c_str()
            << DataFilters.c_str()
            << "All Files (*.*)";
    //    filters << "All Supported Files (*.ves *.osg *.ive *.stl *.wrl *.iv *.obj *.pfb *.flt *.dxf *.3ds *.vtk *.vtu *.vts *.vti *.vtm *.vtp *.vtr *.param *.ens *.case *.mfix *.cas *.avs *.dcm)"
    //            << "VES Files (*.ves)"
    //            << "Geometry files (*.osg *.ive *.stl *.wrl *.iv *.obj *.pfb *.flt *.dxf *.3ds)"
    //            << "Data files (*.vtk *.vtu *.vts *.vti *.vtm *.vtp *.vtr *.param *.ens *.case *.mfix *.cas *.avs *.dcm *.stl)"
    //            << "All Files (*.*)";
    mFileDialog->setNameFilters( filters );

    QObject::connect( mFileDialog, SIGNAL( filesSelected( const QStringList& ) ),
                      this, SLOT( onFileOpenSelected( const QStringList& ) ) );
    QObject::connect( mFileDialog, SIGNAL( rejected() ), this,
                      SLOT( onFileCancelled() ) );
    connect( mFileDialog, SIGNAL( filterSelected( const QString& ) ),
             this, SLOT( OnOpenFileFilterSelected( const QString& ) ) );
    connect( mFileDialog, SIGNAL( currentChanged( const QString& ) ),
             this, SLOT( OnCurrentChanged( const QString& ) ) );

    ActivateTab( AddTab( mFileDialog, "Open File" ) );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::OnCurrentChanged( const QString& path )
{
    // Determine the file extension. If it is ".ves", force single selection.
    // Otherwise, allow multiple selection.
    if( path.endsWith( ".ves", Qt::CaseInsensitive ) )
    {
        // Unselect any other files by selecting only the .ves file
        mFileDialog->selectFile( path );
        mFileDialog->setFileMode( QFileDialog::ExistingFile );
    }
    else
    {
        mFileDialog->setFileMode( QFileDialog::ExistingFiles );
    }
}

////////////////////////////////////////////////////////////////////////////////
void MainWindow::OnOpenFileFilterSelected( const QString& filter )
{
    if( filter == "VES Files (*.ves)" )
    {
        // Force single selection
        mFileDialog->setFileMode( QFileDialog::ExistingFile );
    }
    else
    {
        // Allow multiple selection
        mFileDialog->setFileMode( QFileDialog::ExistingFiles );
    }
}

////////////////////////////////////////////////////////////////////////////////
void MainWindow::onFileOpenSelected( const QStringList& fileNames )
{
    // Close out the fileDialog tab and kill the file dialog
    RemoveTab( mFileDialog );
    if( mFileDialog != 0 )
    {
        mFileDialog->close();
        mFileDialog = 0;
    }

    RemoveTab( m_recentTab );
    if( m_recentTab != 0 )
    {
        m_recentTab->close();
        m_recentTab = 0;
    }

    // Now deal with loading the selected files
    for( int index = 0; index < fileNames.size(); index++ )
    {
        /*
        QLabel* m_loading = new QLabel();
        //m_loadNotifiers.push_back( m_loading );
        QString text("Loading ");
        m_loading->setStyleSheet( "font: bold 12px;" );
        m_loading->setAlignment( Qt::AlignCenter );
        m_loading->setWordWrap( true );
        m_loading->setMaximumSize( this->width() - 20, m_loading->maximumHeight() );
        ActivateTab( AddTab( m_loading, "Loading..." ) );
        */

        QString fileName = fileNames.at( index );
        QDir dir = QDir::current();
        fileName = dir.relativeFilePath( fileName );
        boost::filesystem::path file( fileName.toStdString() );

        /*m_loadNotifiers[ file.filename().string() ] = m_loading;

        // Insert spaces on either side of slashes to allow better wordwrapping
        // in the notifier

        fileName.replace(QString("/"), QString(" / "));
        fileName.replace(QString("\\"), QString(" \\ "));
        text = text + fileName + " ...";
        m_loading->setText( text );
        */

        std::string extension( boost::filesystem::extension( file ) );

        if( !extension.compare( ".ves" ) )
        {
            // It's a ves file, likely with a network

            QSettings settings( QSettings::IniFormat, QSettings::UserScope,
                                "VE Suite", "VE Xplorer" );
            settings.setFallbacksEnabled( false );
            QStringList files = settings.value( "recentProjectList" ).toStringList();
            files.removeAll( fileNames.at( index ) );
            files.prepend( fileNames.at( index ) );
            while( files.size() > 10 )
            {
                files.removeLast();
            }
            settings.setValue( "recentProjectList", files );

            ves::conductor::NetworkLoader* loader =
                ves::conductor::NetworkLoader::createNetworkLoader();
            loader->LoadVesFile( file.string() );
            // Destructor for loader is private; object autodeletes when done
            // processing.

            // Keep the absolute filepath since CWD may change out from under us
            m_saveFileName = fileNames.at( index );
        }
        // Remove the dot from the head of extension so we can compare to our
        // vectors of other file extensions
        extension.erase( extension.begin() );
        std::vector< std::string >::const_iterator iter =
            std::find( m_GeometryExtensions.begin(), m_GeometryExtensions.end(), extension );
        if( iter != m_GeometryExtensions.end() )
        {
            QSettings settings( QSettings::IniFormat, QSettings::UserScope,
                                "VE Suite", "VE Xplorer" );
            settings.setFallbacksEnabled( false );
            QStringList files = settings.value( "recentCADList" ).toStringList();
            files.removeAll( fileNames.at( index ) );
            files.prepend( fileNames.at( index ) );
            while( files.size() > 10 )
            {
                files.removeLast();
            }
            settings.setValue( "recentCADList", files );

            LoadGeometryFile( file.string() );
        }

        iter = std::find( m_DataExtensions.begin(), m_DataExtensions.end(), extension );
        if( iter != m_DataExtensions.end() )
        {
            QSettings settings( QSettings::IniFormat, QSettings::UserScope,
                                "VE Suite", "VE Xplorer" );
            settings.setFallbacksEnabled( false );
            QStringList files = settings.value( "recentDataList" ).toStringList();
            files.removeAll( fileNames.at( index ) );
            files.prepend( fileNames.at( index ) );
            while( files.size() > 10 )
            {
                files.removeLast();
            }
            settings.setValue( "recentDataList", files );

            LoadDataFile( file.string() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::LoadGeometryFile( std::string filename )
{
    // Un-highlight any currently-highlighted geometry
    ves::xplorer::DeviceHandler::instance()->UnselectObjects();

    ves::conductor::CADFileLoader loader;
    std::string parentID;
    ves::xplorer::Model* activeModel =
        ves::xplorer::ModelHandler::instance()->GetActiveModel();

    // If there's no active model, we need to set one up. This requires some
    // gymnastics to get the working directory set properly.
    if( !activeModel )
    {
        // Take the path to the selected Geometry file and use this as the
        // working directory.
        QString filePath( filename.c_str() );
        QString currentDir( QDir::currentPath() );
        filePath = currentDir + "/" + filePath;
        QDir dir( filePath );
        dir.setPath( dir.absolutePath( ) );
        boost::filesystem::path tmp( dir.path().toStdString() );
        filePath = filePath.fromStdString( tmp.remove_filename().string() );
        this->on_actionNew_triggered( filePath );
        activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();

        // Now the filename passed in can be shortened to just the leaf.
        boost::filesystem::path tmp2( filename );
        filename = tmp2.filename().string();
    }

    ModelCADHandler* m_cadHandler = activeModel->GetModelCADHandler();

    parentID = mScenegraphTreeTab->GetSelectedNodeID();
    if( ( parentID.empty() ) || ( !m_cadHandler->AssemblyExists( parentID ) ) )
    {
        // If no valid node is selected, or if the selected node is
        // not an assembly, get or create the top-level assembly for
        // the active model.
        parentID = activeModel->GetModelData()->AddGeometry()->GetID();
    }

    // If AddGeometry call above turned out to be creation, do the
    // rest necessary for proper creation.
    if( !m_cadHandler->AssemblyExists( parentID ) )
    {
        m_cadHandler->CreateAssembly( parentID );
        scenegraph::DCS* assembly = m_cadHandler->GetAssembly( parentID );
        assembly->SetCADPart( activeModel->GetModelData()->GetGeometry() );
        assembly->SetModelData( activeModel->GetModelData() );
        m_cadHandler->GetAssembly( parentID )->setName( "Model_Geometry" );
        m_cadHandler->SetRootCADNodeID( parentID );

        osg::Node::DescriptionList descriptorsList;
        descriptorsList.push_back( "VE_XML_ID" );
        descriptorsList.push_back( parentID );
        descriptorsList.push_back( "Assembly" );
        descriptorsList.push_back( "VE_XML_MODEL_ID" );
        descriptorsList.push_back( activeModel->GetID() );

        ves::xplorer::scenegraph::DCS* assemblyNode =
            m_cadHandler->GetAssembly( parentID );
        assemblyNode->setDescriptions( descriptorsList );

        //Add the top level CAD to the VEBaseClass
        activeModel->GetDCS()->addChild( m_cadHandler->GetAssembly( parentID ) );
    }
    loader.LoadCADFile( filename, parentID );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::LoadDataFile( std::string filename )
{
    // If there's no active model, we need to set one up. This requires some
    // gymnastics to get the working directory set properly.
    if( !ves::xplorer::ModelHandler::instance()->GetActiveModel() )
    {
        QString filePath( filename.c_str() );
        QString currentDir( QDir::currentPath() );
        filePath = currentDir + "/" + filePath;
        QDir dir( filePath );
        dir.setPath( dir.absolutePath( ) );
        boost::filesystem::path tmp( dir.path().toStdString() );
        filePath = filePath.fromStdString( tmp.remove_filename().string() );
        on_actionNew_triggered( filePath );
        // Now the filename passed in can be shortened to just the leaf.
        boost::filesystem::path tmp2( filename );
        filename = tmp2.filename().string();
    }

    ves::open::xml::ParameterBlockPtr mParamBlock;
    ves::open::xml::model::ModelPtr m_veModel(
        ves::xplorer::ModelHandler::instance()->GetActiveModel()->GetModelData() );
    //---------------From OnInformationPacketAdd
    mParamBlock = m_veModel->GetInformationPacket( -1 );
    mParamBlock->SetName( filename );



    //---------------- From OnLoadFile
    //Load a vtk file
    ves::open::xml::DataValuePairPtr tempDVP =
        mParamBlock->GetProperty( "VTK_DATA_FILE" );
    if( !tempDVP )
    {
        tempDVP = mParamBlock->GetProperty( -1 );
    }
    tempDVP->SetData( "VTK_DATA_FILE", filename );

    // RPT: This block isn't doing anything useful yet. Need to figure
    // out proper workflow for determining if the file is part of
    // a transient series.
    /*
    if( ves::xplorer::util::fileIO::getExtension( filename ) == "vtm" )
    {
        boost::filesystem::path tempPath( filename );
        std::string transDir = tempPath.parent_path().string();

        std::vector<std::string> transientFile =
            ves::xplorer::util::fileIO::GetFilesInDirectory(
            transDir, ".vtm" );
        if( transientFile.size() > 0 )
        {
            // ----- TODO: RPT: Need to think about how to deal with this.....
            wxMessageDialog promptDlg( this,
                _( "Is this file part of a transient series?" ),
                _( "Transient Data Chooser" ),
                wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION,
                wxDefaultPosition );
            int answer = promptDlg.ShowModal();
            if( answer == wxID_YES )
            {
                tempDVP =
                    mParamBlock->GetProperty( "VTK_TRANSIENT_SERIES" );
                if( !tempDVP )
                {
                    tempDVP = mParamBlock->GetProperty( -1 );
                }
                //unsigned int translfag = 1;
                tempDVP->SetData( "VTK_TRANSIENT_SERIES", transDir );
            }
        }
    }*/

    ves::xplorer::util::cfdVTKFileHandler tempHandler;
    std::vector< std::string > dataArrayList =
        tempHandler.GetDataSetArraysFromFile( filename );

    if( !dataArrayList.empty() )
    {
        // RPT: For now, we just load all scalars in the file. Need to think
        // about how to choose specific ones from the file.
        //open dialog to choose scalars to load
        //            DataSetDataArrayChoiceDialog choiceDialog( this );
        //            choiceDialog.SetDataArrays( dataArrayList );
        //            if( choiceDialog.ShowModal() == wxID_OK )
        //            {
        //                dataArrayList = choiceDialog.GetUserActiveArrays();
        ves::open::xml::DataValuePairPtr arraysDVP =
            mParamBlock->GetProperty( "VTK_ACTIVE_DATA_ARRAYS" );
        if( !arraysDVP )
        {
            arraysDVP = mParamBlock->GetProperty( -1 );
        }
        ves::open::xml::OneDStringArrayPtr
        stringArray( new ves::open::xml::OneDStringArray() );
        stringArray->SetArray( dataArrayList );
        arraysDVP->SetData( "VTK_ACTIVE_DATA_ARRAYS", stringArray );
    }

    ves::open::xml::DataValuePairSharedPtr
    dataValuePair( new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "CREATE_NEW_DATASETS",
                            ves::open::xml::model::ModelPtr(
                                new ves::open::xml::model::Model( *m_veModel ) ) );

    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "UPDATE_MODEL_DATASETS" ) );
    veCommand->AddDataValuePair( dataValuePair );
    //Add the active dataset name to the command
    ves::open::xml::DataValuePairSharedPtr dataSetName(
        new ves::open::xml::DataValuePair() );
    if( mParamBlock->GetProperty( "VTK_DATA_FILE" ) )
    {
        dataSetName->SetData( "VTK_DATASET_NAME",
                              mParamBlock->GetProperty( "VTK_DATA_FILE" )->GetDataString() );
    }
    else
    {
        dataSetName->SetData( "VTK_DATASET_NAME", "NULL" );
    }
    veCommand->AddDataValuePair( dataSetName );
    ves::xplorer::command::CommandManager::instance( )->AddXMLCommand( veCommand );

    if( m_displayFeatures.contains( "Visualization" ) )
    {
        ActivateTab( AddTab( mVisualizationTab, "Visualization" ) );
    }

    // Unclear why this is needed here, but for some reason mFileDialog is not
    // otherwise being nulled out when loading datasets.
    mFileDialog = 0;
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::onFileCancelled()
{
    RemoveTab( mFileDialog );

    if( mFileDialog )
    {
        mFileDialog->close();
        mFileDialog = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionSave_triggered()
{
    if( m_saveFileName.isEmpty() )
    {
        on_actionSaveAs_triggered();
    }
    else
    {
        onFileSaveSelected( m_saveFileName );
    }
}

////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionSaveAs_triggered()
{
    // Don't allow multiple file dialogs to be opened.
    if( mFileDialog )
    {
        mFileDialog->close();
        mFileDialog = 0;
    }

    mFileDialog = new QFileDialog( 0 );
    // Ensure that we use Qt's internal file dialog class since native file
    // dialogs cannot be embedded in a QTabWidget
    mFileDialog->setOptions( QFileDialog::DontUseNativeDialog | QFileDialog::DontConfirmOverwrite );
    // Make mFileDialog manage its own lifetime and memory
    mFileDialog->setAttribute( Qt::WA_DeleteOnClose );
    mFileDialog->setFileMode( QFileDialog::AnyFile );
    QStringList filters;
    filters << "VES Files (*.ves)";
    mFileDialog->setNameFilters( filters );

    mFileDialog->setAcceptMode( QFileDialog::AcceptSave );

    QObject::connect( mFileDialog, SIGNAL( fileSelected( const QString& ) ),
                      this, SLOT( onFileSaveSelected( const QString& ) ) );
    QObject::connect( mFileDialog, SIGNAL( rejected() ), this,
                      SLOT( onFileCancelled() ) );

    ActivateTab( AddTab( mFileDialog, "Save Session" ) );
}

////////////////////////////////////////////////////////////////////////////////
void MainWindow::onFileSaveSelected( const QString& fileName )
{
    LOG_TRACE( "onFileSaveSelected" );
    m_saveFileName = fileName;

    // Close out the fileDialog tab and kill the file dialog
    RemoveTab( mFileDialog );

    if( mFileDialog != 0 )
    {
        mFileDialog->close();
        mFileDialog = 0;
    }

    //std::cout << "Telling DatabaseManager to save off to " << fileName.toStdString() << std::endl << std::flush;

    QString tFileName( fileName );
    // If the filename ends in ".ves", break that off
    if( tFileName.endsWith( ".ves", Qt::CaseInsensitive ) )
    {
        tFileName.chop( 4 );
    }
    std::string pathName = tFileName.toStdString();

    // Save the database as fileName.db
    std::string dbFileName = pathName;
    dbFileName.append( ".db" );
    ves::xplorer::data::DatabaseManager::instance()->SaveAs( dbFileName );

    // Set the database name in system
    QString tempName;
    tempName = tempName.fromStdString( dbFileName );
    QDir relativeDir = QDir::current();
    tempName = relativeDir.relativeFilePath( tempName );
    dbFileName = tempName.toStdString();
    ves::xplorer::Model* model =
        ves::xplorer::ModelHandler::instance()->GetActiveModel();
    ves::open::xml::model::SystemPtr system = model->GetModelData()->GetParentSystem();
    system->SetDBReference( dbFileName );
    LOG_DEBUG( "onFileSaveSelected: setting database filename to " << dbFileName );

    // Save off the .ves file
    std::string vesFileName = pathName;
    vesFileName.append( ".ves" );
    //ves::conductor::XMLDataBufferEngine::instance()->SaveVESData( vesFileName );
    SaveSytemToFile( system, vesFileName );

    // Put this file at the top of the RecentFiles projects list
    QSettings settings( QSettings::IniFormat, QSettings::UserScope,
                        "VE Suite", "VE Xplorer" );
    settings.setFallbacksEnabled( false );
    QStringList files = settings.value( "recentProjectList" ).toStringList();
    files.removeAll( QString::fromStdString( vesFileName ) );
    files.prepend( QString::fromStdString( vesFileName ) );
    settings.setValue( "recentProjectList", files );

    if( m_recentTab )
    {
        m_recentTab->RefreshFiles();
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::SaveSytemToFile( ves::open::xml::model::SystemPtr system, std::string fileName )
{
    LOG_TRACE( "SaveSystemToFile: " << fileName );
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >(
                         system, "veSystem" ) );

    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.WriteXMLDocument( nodes, fileName, "Network" );
}

////////////////////////////////////////////////////////////////////////////////
void MainWindow::OnActiveModelChanged( const std::string& modelID )
{
    // emit Qt-signal which is connected to QueuedOnActiveModelChanged.
    // A queued connection is necessary because widgets are altered during
    // this event.
    ActiveModelChanged( modelID );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::QueuedOnActiveModelChanged( const std::string& modelID )
{
    boost::ignore_unused_variable_warning( modelID );

    // We get rid of all existing tabs, then open only those appropriate
    // to the active model.

    const std::string LastKnownActive = mActiveTab;

    //RemoveAllTabs();

    //Put the preferences tab first
    AddTab( m_preferencesTab, "Preferences" );

    if( m_displayFeatures.contains( "Plugins" ) )
    {
        AddTab( m_pluginsTab, "Plugins" );
    }

    ves::xplorer::Model* model =
        ves::xplorer::ModelHandler::instance()->GetActiveModel();

    // Show visualization tab?
    if( m_displayFeatures.contains( "Visualization" ) )
    {
        //Only if we have datasets
        if( model->GetNumberOfCfdDataSets() > 0 )
        {
            AddTab( mVisualizationTab, "Visualization" );
        }
    }

    // Show the scenegraph tree
    if( m_displayFeatures.contains( "Tree" ) )
    {
        AddTab( mScenegraphTreeTab, "Layers" );
    }

    // Reactivate the last-known active tab
    // If there was no active tab, activate vis tab, if it exists
    if( LastKnownActive.empty() || ( LastKnownActive == "" && model->GetNumberOfCfdDataSets() > 0 ) )
    {
        if( m_displayFeatures.contains( "Visualization" ) )
        {
            ActivateTab( "Visualization" );
        }
    }
    else
    {
        ActivateTab( LastKnownActive );
    }
}
////////////////////////////////////////////////////////////////////////////////
#ifdef MINERVA_GIS_SUPPORT
void MainWindow::on_actionAdd_Planet_triggered( bool )
{
    ves::xplorer::minerva::MinervaManager::instance()->AddEarthToScene();

    if( mMinervaStackedWidget )
    {
        on_actionRemove_Planet_triggered( false );
    }

    if( m_displayFeatures.contains( "GIS" ) )
    {
        mMinervaStackedWidget = new ves::conductor::qt::minerva::StackedWidget();
        mMinervaStackedWidget->
        setFeature( ves::xplorer::minerva::MinervaManager::instance()->
                    GetTileEngineBody()->container() );
        ActivateTab( AddTab( mMinervaStackedWidget, "Minerva Layers" ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionRemove_Planet_triggered( bool )
{
    ves::xplorer::minerva::MinervaManager::instance()->Clear();

    if( mMinervaStackedWidget )
    {
        RemoveTab( mMinervaStackedWidget );
        delete mMinervaStackedWidget;
        mMinervaStackedWidget = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionConfigure_Layers_triggered( bool )
{
    if( !mMinervaStackedWidget )
    {
        mMinervaStackedWidget = new ves::conductor::qt::minerva::StackedWidget();
        mMinervaStackedWidget->
            setFeature( ves::xplorer::minerva::MinervaManager::instance()->
                GetTileEngineBody()->container() );
    }

    ui->tabWidget->
           setCurrentIndex( AddTab( mMinervaStackedWidget, "Minerva Layers" ) );
}
#else
void MainWindow::on_actionAdd_Planet_triggered( bool )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionRemove_Planet_triggered( bool )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionConfigure_Layers_triggered( bool )
{
    ;
}
#endif
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionStepPhysics_triggered()
{
    scenegraph::PhysicsSimulator::instance()->SetIdle( true );
    scenegraph::PhysicsSimulator::instance()->StepSimulation();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionPlayPhysics_triggered()
{
    scenegraph::PhysicsSimulator::instance()->SetIdle( false );
    ves::xplorer::ModelHandler::instance()->PlayCADAnimations();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionPausePhysics_triggered()
{
    scenegraph::PhysicsSimulator::instance()->SetIdle( true );
    ves::xplorer::ModelHandler::instance()->PauseCADAnimations();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionResetPhysics_triggered()
{
    scenegraph::PhysicsSimulator::instance()->SetIdle( true );
    scenegraph::PhysicsSimulator::instance()->ResetScene();
    ves::xplorer::ModelHandler::instance()->ResetCADAnimations();
}
////////////////////////////////////////////////////////////////////////////////
/*void MainWindow::on_actionDebugPhysics_triggered( bool trigger )
{
    //scenegraph::PhysicsSimulator::instance()->SetDebuggingOn( toggle );
}*/
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionManipulatorStack_triggered()
{
    if( m_manipulatorMenuStack->isVisible() )
    {
        m_manipulatorMenuStack->hide();
    }
    else
    {
        m_manipulatorMenuStack->Show();
    }
}
////////////////////////////////////////////////////////////////////////////////
//void MainWindow::on_actionManipulatorStack_hovered()
//{
//    if( m_manipulatorMenuStack->isVisible() )
//    {
//        m_manipulatorMenuStack->hide();
//    }
//    else
//    {
//        m_manipulatorMenuStack->Show();
//    }
//}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionScaleManipulator_triggered()
{
    scenegraph::manipulator::ManipulatorManager& manipulatorManager =
        scenegraph::SceneManager::instance()->GetManipulatorManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        manipulatorManager.GetSceneManipulator();
    sceneManipulator->SetEnabledModes(
        scenegraph::manipulator::TransformationType::SCALE_COMPOUND );
    sceneManipulator->DefaultForm();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionTranslateManipulator_triggered()
{
    scenegraph::manipulator::ManipulatorManager& manipulatorManager =
        scenegraph::SceneManager::instance()->GetManipulatorManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        manipulatorManager.GetSceneManipulator();
    sceneManipulator->SetEnabledModes(
        scenegraph::manipulator::TransformationType::TRANSLATE_COMPOUND );
    sceneManipulator->DefaultForm();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionRotateManipulator_triggered()
{
    scenegraph::manipulator::ManipulatorManager& manipulatorManager =
        scenegraph::SceneManager::instance()->GetManipulatorManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        manipulatorManager.GetSceneManipulator();
    sceneManipulator->SetEnabledModes(
        scenegraph::manipulator::TransformationType::ROTATE_COMPOUND );
    sceneManipulator->DefaultForm();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionComboManipulator_triggered()
{
    scenegraph::manipulator::ManipulatorManager& manipulatorManager =
        scenegraph::SceneManager::instance()->GetManipulatorManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        manipulatorManager.GetSceneManipulator();
    sceneManipulator->SetEnabledModes(
        scenegraph::manipulator::TransformationType::ALL );
    sceneManipulator->ComboForm();
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionEnableManipulator_triggered( bool triggered )
{
    scenegraph::manipulator::ManipulatorManager& manipulatorManager =
        scenegraph::SceneManager::instance()->GetManipulatorManager();
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        manipulatorManager.GetSceneManipulator();

    manipulatorManager.Enable( triggered );
    sceneManipulator->Enable( triggered );

    if( triggered )
    {
        if( ves::xplorer::DeviceHandler::instance()->GetSelectedDCS() )
        {
            sceneManipulator->Show();
        }
    }
    else
    {
        sceneManipulator->Hide();
    }

    ui->actionScaleManipulator->setEnabled( triggered );
    ui->actionTranslateManipulator->setEnabled( triggered );
    ui->actionRotateManipulator->setEnabled( triggered );
    ui->actionComboManipulator->setEnabled( triggered );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionNavigationStack_triggered()
{
    if( m_navMenuStack->isVisible() )
    {
        m_navMenuStack->hide();
    }
    else
    {
        m_navMenuStack->Show();
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionSmallJump_hovered()
{
    m_jumpSignal.signal( "Small" );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionMediumJump_triggered()
{
    m_jumpSignal.signal( "Medium" );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionLargeJump_triggered()
{
    m_jumpSignal.signal( "Large" );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionBoundingBoxJump_triggered()
{
    m_jumpSignal.signal( "Bounding Box" );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionWorldNavigation_triggered()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionObjectNavigation_triggered()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionCharacterNavigation_triggered( bool triggered )
{
    scenegraph::CharacterController& characterController =
        scenegraph::SceneManager::instance()->GetCharacterController();
    characterController.Enable( triggered );

    ui->actionCharacterFlyMode->setEnabled( !triggered );

    m_characterEnable.signal( triggered );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionCharacterFlyMode_triggered( bool triggered )
{
    scenegraph::CharacterController& characterController =
        scenegraph::SceneManager::instance()->GetCharacterController();

    characterController.EnableFlying( triggered );

    ui->actionCharacterNavigation->setEnabled( !triggered );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionGISStack_triggered( )
{
    if( m_gisMenuStack->isVisible() )
    {
        m_gisMenuStack->hide();
    }
    else
    {
        m_gisMenuStack->Show();
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionViewStack_triggered( )
{
    if( m_viewMenuStack->isVisible() )
    {
        m_viewMenuStack->hide();
    }
    else
    {
        m_viewMenuStack->Show();
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionShowPluginsTab_triggered()
{
    if( m_displayFeatures.contains( "Plugins" ) )
    {
        int index = AddTab( m_pluginsTab, "Plugins" );

        // Coming soon...
        //    QToolButton* tb = new QToolButton;
        //    tb->setText( "x" );
        //    tb->setAutoRaise( true );
        //    ui->tabWidget->SetTabButton( index, tb );
        ActivateTab( index );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionShowPreferencesTab_triggered()
{
    ActivateTab( AddTab( m_preferencesTab, "Preferences" ) );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionNew_triggered( const QString& workingDir )
{
    onRecentFileRejected();
    // If workingDir is empty, this method was called by clicking the "New File"
    // icon. If workingDir is not empty, this method was called by successful
    // completion of the "Select Working Directory" file dialog, which is
    // initiated below.
    if( workingDir.isEmpty() )
    {
        // Don't allow multiple file dialogs to be opened.
        if( mFileDialog != 0 )
        {
            return;
        }

        mFileDialog = new QFileDialog( 0 );
        // Ensure that we use Qt's internal file dialog class since native file
        // dialogs cannot be embedded in a QTabWidget
        mFileDialog->setOptions( QFileDialog::DontUseNativeDialog );
        // Make mFileDialog manage its own lifetime and memory
        mFileDialog->setAttribute( Qt::WA_DeleteOnClose );
        mFileDialog->setFileMode( QFileDialog::DirectoryOnly );

        mFileDialog->setAcceptMode( QFileDialog::AcceptOpen );

        QObject::connect( mFileDialog, SIGNAL( fileSelected( const QString& ) ),
                          this, SLOT( on_actionNew_triggered( const QString& ) ) );
        QObject::connect( mFileDialog, SIGNAL( rejected() ), this,
                          SLOT( onFileCancelled() ) );

        ActivateTab( AddTab( mFileDialog, "Select Working Directory" ) );
        return;
    }

    m_saveFileName.clear();

    // Change to the new working directory
    std::cout << "Setting working directory to "
              << workingDir.toStdString() << std::endl << std::flush;
    QDir::setCurrent( workingDir );

    reinterpret_cast< ves::util::StringSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "WorkingDirectoryChanged" ) )
    ->signal( workingDir.toStdString() );

    // Close out the fileDialog tab and kill the file dialog
    RemoveTab( mFileDialog );

    if( mFileDialog != 0 )
    {
        mFileDialog->close();
        mFileDialog = 0;
    }

    // Let xplorer know we are loading a new ves file so that it can do any
    // necessary cleanup, such as resetting the database
    reinterpret_cast< ves::util::StringSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "VesFileLoading" ) )
    ->signal( "New" );

    XMLDataBufferEngine* mDataBufferEngine = XMLDataBufferEngine::instance();

    mDataBufferEngine->NewVESData( true );
    ///Initialize top level network
    ves::open::xml::model::NetworkPtr tempNetwork(
        new ves::open::xml::model::Network() );

    mDataBufferEngine->GetXMLSystemDataObject(
        mDataBufferEngine->GetTopSystemId() )->AddNetwork( tempNetwork );

    using namespace ves::open::xml::model;

    SystemPtr system( mDataBufferEngine->GetXMLSystemDataObject(
                          mDataBufferEngine->GetTopSystemId() ) );

    ModelPtr mod( new Model );
    mod->SetPluginType( "DefaultPlugin" );
    mod->SetPluginName( "DefaultPlugin" );
    mod->SetVendorName( "DefaultPlugin" );
    mod->SetParentSystem( system );

    system->AddModel( mod );

    const std::string network = XMLDataBufferEngine::instance()->
                                SaveVESData( std::string( "returnString" ) );

    ves::xplorer::network::GraphicalPluginManager::instance()->
    SetCurrentNetwork( network );

    ves::xplorer::network::GraphicalPluginManager::instance()->LoadDataFromCE();

    ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
        ves::xplorer::scenegraph::SceneManager::instance()->GetActiveNavSwitchNode() );

    ves::xplorer::ModelHandler::instance()->SetActiveModel( mod->GetID() );

    // Force CAD tree to re-read the now "empty" scenegraph
    mScenegraphTreeTab->PopulateWithRoot(
        &( ves::xplorer::scenegraph::SceneManager::instance()->
           GetGraphicalPluginManager() ) );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::RemoveNotifier( const std::string& filename )
{
    RemoveNotifierQSignal( filename );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::QueuedRemoveNotifier( std::string const& filename )
{
    std::map< std::string, QLabel* >::iterator iter =
        m_loadNotifiers.find( filename );
    if( iter != m_loadNotifiers.end() )
    {
        RemoveTab( iter->second );
        delete iter->second;
        m_loadNotifiers.erase( iter );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::UseAsSurfaceData( const std::string& uuid, bool flag )
{
    UseAsSurfaceDataQSignal( uuid, flag );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::UseAsSurfaceDataQueued( const std::string uuid, bool flag )
{
    ves::xplorer::data::CADPropertySet cad;
    cad.SetUUID( uuid );
    cad.LoadFromDatabase();
    std::string filename =
        boost::any_cast<std::string>( cad.GetPropertyValue( "Filename" ) );

    if( flag )
    {
        LoadDataFile( filename );
    }
    else
    {
        ves::xplorer::ModelHandler::instance()->
        GetActiveModel()->DeleteDataSet( filename );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionRecent_triggered()
{
    if( m_recentTab != 0 )
    {
        ActivateTab( AddTab( m_recentTab, "Recent Files" ) );
    }
    else
    {
        m_recentTab = new RecentFiles( 0 );
        connect( m_recentTab, SIGNAL( fileSelected( QString ) ),
                 this, SLOT( onRecentFileSelected( QString ) ) );
        connect( m_recentTab, SIGNAL( newProject() ),
                 this, SLOT( on_actionNew_triggered() ) );
        connect( m_recentTab, SIGNAL( openProject() ),
                 this, SLOT( on_actionOpen_triggered() ) );
        connect( m_recentTab, SIGNAL( rejected() ),
                 this, SLOT( onRecentFileRejected() ) );
        ActivateTab( AddTab( m_recentTab, "Recent Files" ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::onRecentFileSelected( QString file )
{
    RemoveTab( m_recentTab );
    if( m_recentTab != 0 )
    {
        m_recentTab->close();
    }
    m_recentTab = 0;

    if( file == "" )
    {
        return;
    }

    // Repackage the QString as a QStringList since that is what
    // onFileOpenSelected requires.
    QStringList files;
    files << file;
    onFileOpenSelected( files );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::onRecentFileRejected()
{
    RemoveTab( m_recentTab );
    if( m_recentTab != 0 )
    {
        m_recentTab->close();
    }
    m_recentTab = 0;
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionConstraints_triggered()
{
    if( m_displayFeatures.contains( "Constraints" ) )
    {
        ActivateTab( AddTab( m_constraintsTab, "Constraints" ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_tabWidget_tabCloseRequested( int index )
{
    RemoveTab( ui->tabWidget->tabText( index ).toStdString() );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::SetLogSplitter( Poco::SplitterChannel* splitter )
{
    boost::ignore_unused_variable_warning( splitter );
    //m_qtLogView = new QtLogChannel();
    //splitter->addChannel( m_qtLogView );
}
////////////////////////////////////////////////////////////////////////////////
void MainWindow::on_actionShowCameraTab_triggered()
{
    ActivateTab( AddTab( m_cameraTab, "Cameras" ) );
}
////////////////////////////////////////////////////////////////////////////////

// TESTPLOT
#if 1
#include <qwt_plot_curve.h>
#include <qwt_plot_grid.h>
#include <qwt_plot.h>
#include <ves/conductor/qt/UIManager.h>
#include <ves/conductor/qt/UIElementQt.h>
void MainWindow::on_actionShowTestPlot_triggered()
{
    QwtPlot* plot = new QwtPlot( 0 );
    plot->setTitle( "Example Qwt Plot" );
    plot->setAxisTitle( QwtPlot::xBottom, "X Data" );
    plot->setAxisTitle( QwtPlot::yLeft, "Y Data" );

    QwtPlotCurve* curve = new QwtPlotCurve( "Curve" );
    curve->setPen( QPen( Qt::red ) );

    double x_data[3] = {1, 2, 7};
    double y_data[3] = {1, 2, 3};

    curve->setSamples( x_data, y_data, 3 );

    QwtPlotGrid* grid_y = new QwtPlotGrid();
    grid_y->attach( plot );

    curve->attach( plot );

    plot->setAutoReplot( true );

    // In a normal program, we'd do this to display the plot in its own window:
    // plot->show();
    // but we need the new window to be managed as a UIElement, so we do this:
    ves::conductor::UIElementQt* element = new ves::conductor::UIElementQt();
    element->SetInitialImageWidthAndHeight( 400, 300 );
    element->SetWidget( plot );
    ves::conductor::UIManager::instance()->AddElement( element );
}
#else
void MainWindow::on_actionShowTestPlot_triggered()
{

}
#endif

////////////////////////////////////////////////////////////////////////////////
