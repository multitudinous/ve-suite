/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/conductor/qt/ui_MainWindow.h>


#include <ves/conductor/qt/propertyBrowser/Visualization.h>
#include<ves/conductor/qt/NetworkLoader.h>

#include <ves/xplorer/eventmanager/SlotWrapper.h>
#include <ves/xplorer/eventmanager/EventManager.h>

#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>

#include <iostream>
#include <QtGui/QPaintEvent>

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    
    ui->mainToolBar->addAction(ui->actionFile);
    
    ui->menuBar->close();
    
    mFileOpsStack = new IconStack( ui->mainToolBar->widgetForAction( ui->actionFile ), this );
    mFileOpsStack->AddAction( ui->actionNew );
    mFileOpsStack->AddAction( ui->actionOpen);
    mFileOpsStack->AddAction( ui->actionSave );

    // Make sure there is no statusbar on this widget.
    setStatusBar(0);
    
    // Create set of default dialogs that can be added as tabs
    mVisualizationTab = new ves::conductor::Visualization( 0 );

    // Connect to the ActiveModelChangedSignal so we can show the correct 
    // tabs when the model changes
    typedef boost::signals2::signal< void ( const std::string& ) > AMCSignal_type;
    AMCSignal_type::slot_type slotFunctor( boost::bind( &MainWindow::OnActiveModelChanged, this, _1 ) );
    ves::xplorer::eventmanager::SlotWrapper< AMCSignal_type > slotWrapper( slotFunctor );
    ves::xplorer::eventmanager::EventManager::instance( )->ConnectSignal( "ModelHandler.ActiveModelChangedSignal", &slotWrapper, mConnections );
}

MainWindow::~MainWindow()
{
    delete ui;
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
    mFileDialog->setNameFilter(tr("VES Files (*.ves)"));
    
    QObject::connect( mFileDialog, SIGNAL(fileSelected(const QString &)), 
                      this, SLOT(onFileOpenSelected(QString)) );
    QObject::connect( mFileDialog, SIGNAL(rejected()), this,
                      SLOT( onFileCancelled() ) );
                      
    ui->tabWidget->setCurrentIndex( AddTab( mFileDialog, "Open File" ) );
}

void MainWindow::onFileOpenSelected( QString fileName )
{
    ves::conductor::NetworkLoader loader;
    loader.LoadVesFile( fileName.toStdString() );
    
    RemoveTab( mFileDialog );
    
    if (mFileDialog)
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
    // We will get rid of all existing tabs, then open only those appropriate 
    // to the active model.
    
    RemoveAllTabs();
    
    // Not sure if this will work on Mac or Windows....
    ves::xplorer::Model* model =  
        ves::xplorer::ModelHandler::instance()->GetActiveModel( );
       
    if( model->GetNumberOfCfdDataSets() > 0 )
    {
        AddTab( mVisualizationTab, "Visualization" );
    }
}
    
