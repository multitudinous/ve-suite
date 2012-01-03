/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include <ves/conductor/qt/ui_MainWindow.h>
#include <QtGui/QPaintEvent>

#include <ves/conductor/qt/propertyBrowser/Visualization.h>
#include <ves/conductor/qt/NetworkLoader.h>
#include<ves/conductor/qt/CADFileLoader.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/open/xml/Command.h>

#include <ves/xplorer/eventmanager/SlotWrapper.h>
#include <ves/xplorer/eventmanager/EventManager.h>

#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#ifdef MINERVA_GIS_SUPPORT
# include <ves/xplorer/minerva/MinervaManager.h>
# include <ves/conductor/qt/minerva/LayersTree.h>

#include <Minerva/Core/TileEngine/Body.h>
#endif

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>

#include <iostream>

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    mActiveTab( "" ),
    mLayersTree ( 0x0 )
{
    ui->setupUi(this);
    
    ui->mainToolBar->addAction(ui->actionFile);
    
    //ui->menuBar->close();
    
    mFileOpsStack = new IconStack( ui->mainToolBar->widgetForAction( ui->actionFile ), this );
    mFileOpsStack->AddAction( ui->actionNew );
    mFileOpsStack->AddAction( ui->actionOpen);
    mFileOpsStack->AddAction( ui->actionSave );

    // Make sure there is no statusbar on this widget.
    setStatusBar(0);
    
    // Create set of default dialogs that can be added as tabs
    mVisualizationTab = new ves::conductor::Visualization( 0 );
    mScenegraphTreeTab = new ves::conductor::TreeTab();

    // Connect to the ActiveModelChangedSignal so we can show the correct 
    // tabs when the model changes
    {
        typedef boost::signals2::signal< void ( const std::string& ) > AMCSignal_type;
        AMCSignal_type::slot_type slotFunctor( boost::bind( &MainWindow::OnActiveModelChanged, this, _1 ) );
        ves::xplorer::eventmanager::SlotWrapper< AMCSignal_type > slotWrapper( slotFunctor );
        ves::xplorer::eventmanager::EventManager::instance( )->ConnectSignal( "ModelHandler.ActiveModelChangedSignal", &slotWrapper, mConnections );
    }
    
    // Connect to ObjectPickedSignal so we can update the scenegraph tree view when
    // an object is picked
    {
        typedef boost::signals2::signal< void ( osg::NodePath& ) > ObjectPickedSignal_type;
        ObjectPickedSignal_type::slot_type slotFunctor( boost::bind( &MainWindow::OnObjectPicked, this, _1 ) );
        ves::xplorer::eventmanager::SlotWrapper< ObjectPickedSignal_type > slotWrapper( slotFunctor );
        ves::xplorer::eventmanager::EventManager::instance( )->ConnectSignal( "KeyboardMouse.ObjectPickedSignal", &slotWrapper, mConnections );
    }
}

MainWindow::~MainWindow()
{
    delete ui;

#ifdef MINERVA_GIS_SUPPORT
    delete mLayersTree;

    ves::xplorer::minerva::MinervaManager::instance()->Clear();
#endif
}

void MainWindow::changeEvent(QEvent* e)
{
    QMainWindow::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}

int MainWindow::AddTab( QWidget* widget, const std::string& tabLabel )
{
    int index = ui->tabWidget->addTab( widget, 
                                       QString::fromStdString( tabLabel ) );
    mTabbedWidgets[ tabLabel ] = widget;
    return index;
}

void MainWindow::RemoveTab( QWidget* widget )
{
    if( !widget )
    {
        return;
    }
    
    // Remove the visual tab
    ui->tabWidget->removeTab( ui->tabWidget->indexOf( widget ) );
    
    // Remove this tab from mTabbedWidgets map
    std::map< std::string, QWidget* >::iterator iter;
    for( iter = mTabbedWidgets.begin(); iter != mTabbedWidgets.end(); iter++ )
    {
        if( iter->second == widget )
        {
            mTabbedWidgets.erase( iter );
            break;
        }
    }
}

void MainWindow::RemoveTab( const std::string& tabLabel )
{
    std::map< std::string, QWidget* >::iterator marked = mTabbedWidgets.find( tabLabel );
    if( marked != mTabbedWidgets.end() )
    {
        RemoveTab( marked->second );
    }
}

void MainWindow::RemoveAllTabs()
{
    ui->tabWidget->clear();
    mTabbedWidgets.clear();
}

void MainWindow::ActivateTab( QWidget* widget )
{
    // Get the widget's index and call the (int) overloaded version
    ActivateTab( ui->tabWidget->indexOf( widget ) );
}

void MainWindow::ActivateTab( const std::string& tabLabel )
{
    std::map< std::string, QWidget* >::iterator iter = mTabbedWidgets.find( tabLabel );
    if( iter != mTabbedWidgets.end() )
    {
        // Get the associated widget and call the (QWidget*) overloaded version
        ActivateTab( iter->second );
    }
}

/// This version is the final destination of the other overloaded versions.
/// All base functionality should happen here.
void MainWindow::ActivateTab( int index )
{
    if( index < ui->tabWidget->count() )
    {
        ui->tabWidget->setCurrentIndex( index );
    }
}

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

void MainWindow::on_actionFile_triggered()
{
    mFileOpsStack->Show();
}

void MainWindow::on_actionOpen_triggered()
{
    // Don't allow multiple file dialogs to be opened.
    if( mFileDialog )
    {
        return;
    }
    
    mFileDialog = new QFileDialog( 0 );
    // Ensure that we use Qt's internal file dialog class since native file
    // dialogs cannot be embedded in a QTabWidget
    mFileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    // Make mFileDialog manage its own lifetime and memory
    mFileDialog->setAttribute( Qt::WA_DeleteOnClose );
    mFileDialog->setFileMode( QFileDialog::ExistingFile );
    QStringList filters;
    filters << "All Supported Files (*.ves *.osg *.ive *.stl *.wrl *.iv *.obj *.pfb *.flt *.dxf *.3ds)"
            << "VES Files (*.ves)"
            << "OSG files (*.osg *.ive)"
            << "STL files (*.stl)"
            << "VRML/Inventor files (*.wrl *.iv)"
            << "OBJ files (*.obj)"
            << "Performer Binary files (*.pfb)"
            << "Flight files (*.flt)"
            << "DXF files (*.dxf)"
            << "3DS files (*.3ds)"
            << "All Files (*.*)";
    //mFileDialog->setNameFilter(tr("VES Files (*.ves)"));
    mFileDialog->setNameFilters( filters );
    
    QObject::connect( mFileDialog, SIGNAL(fileSelected(const QString &)), 
                      this, SLOT(onFileOpenSelected(QString)) );
    QObject::connect( mFileDialog, SIGNAL(rejected()), this,
                      SLOT( onFileCancelled() ) );
                      
    ActivateTab( AddTab( mFileDialog, "Open File" ) );
}

void MainWindow::onFileOpenSelected( QString fileName )
{
    boost::filesystem::path file( fileName.toStdString() );
    std::string extension( boost::filesystem::extension( file ) );
    
    if( !extension.compare( ".ves" ) )
    {
        // It's a ves file, likely with a network
        ves::conductor::NetworkLoader loader;
        loader.LoadVesFile( file.string() );
    }
    else
    {
        // Assume it's a cad file for now
        ves::conductor::CADFileLoader loader;
        loader.LoadCADFile( file.string() );
    }
    
    RemoveTab( mFileDialog );
    
    if ( mFileDialog != 0 )
    {
        mFileDialog->close();
        mFileDialog = 0;
    }
}

void MainWindow::onFileCancelled()
{
    RemoveTab( mFileDialog );
    
    if (mFileDialog)
    {
        mFileDialog->close();
        mFileDialog = 0;
    }
}

void MainWindow::OnActiveModelChanged( const std::string& modelID )
{   
    // We get rid of all existing tabs, then open only those appropriate 
    // to the active model.

    std::string LastKnownActive = mActiveTab;
    
    RemoveAllTabs();
    
    // Show visualization tab?
    ves::xplorer::Model* model =  
        ves::xplorer::ModelHandler::instance()->GetActiveModel( );
        
    if( model->GetNumberOfCfdDataSets() > 0 )
    {
        AddTab( mVisualizationTab, "Visualization" );
    }
    
    // Show the scenegraph tree
    AddTab( mScenegraphTreeTab, "Scenegraph" );
    
    // Reactivate the last-known active tab
    ActivateTab( LastKnownActive );
}


#ifdef MINERVA_GIS_SUPPORT
void MainWindow::on_actionAdd_Planet_triggered ( bool )
{
  ves::xplorer::minerva::MinervaManager::instance()->AddEarthToScene();

  if ( mLayersTree )
  {
    this->RemoveTab ( mLayersTree );
    delete mLayersTree;
  }

  mLayersTree = new ves::conductor::qt::minerva::LayersTree;
  mLayersTree->buildTree ( ves::xplorer::minerva::MinervaManager::instance()->GetTileEngineBody()->container() );
  ui->tabWidget->setCurrentIndex( AddTab( mLayersTree, "Minerva Layers" ) );
}

void MainWindow::on_actionRemove_Planet_triggered ( bool )
{
  ves::xplorer::minerva::MinervaManager::instance()->Clear();

  if ( mLayersTree )
  {
    this->RemoveTab ( mLayersTree );
    delete mLayersTree;
    mLayersTree = 0x0;
  }
}

void MainWindow::on_actionConfigure_Layers_triggered ( bool )
{
}
#endif

void MainWindow::OnObjectPicked( osg::NodePath& nodePath )
{    
    // This is a bit hackish. Instead of re-reading the scenegraph every time a new
    // selection is made, we should be hooked up to signals for changes to the 
    // scenegraph so we can re-read at the appropriate time. The only operation 
    // that *should* be done here is OpenToAndSelect.
    mScenegraphTreeTab->PopulateWithRoot( ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
    
    mScenegraphTreeTab->OpenToAndSelect( nodePath );
}
