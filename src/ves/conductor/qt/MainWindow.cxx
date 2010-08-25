/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#include <iostream>
#include <QtGui/QPaintEvent>

MainWindow::MainWindow(QWidget* parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    
    ui->mainToolBar->addAction(ui->actionFile);
    
    ui->menuBar->close();
    
    tb = new IconStack( ui->mainToolBar->widgetForAction( ui->actionFile ), this );
    tb->AddAction( ui->actionNew );
    tb->AddAction( ui->actionOpen);
    tb->AddAction( ui->actionSave );

    // Make sure there is no statusbar on this widget.
    setStatusBar(0);
    
    ves::conductor::Visualization* visWindow = new ves::conductor::Visualization( 0 );
    ui->tabWidget->addTab( visWindow, "Visualization" );
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

void MainWindow::on_actionFile_triggered()
{
    tb->Show();
}

void MainWindow::on_actionOpen_triggered()
{
    // Don't allow multiple file dialogs to be opened.
    if( mFileDialog )
    {
        return;
    }
    
    mFileDialog = new QFileDialog( 0 );
    mFileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    mFileDialog->setAttribute( Qt::WA_DeleteOnClose );
    mFileDialog->setFileMode( QFileDialog::ExistingFile );
    mFileDialog->setNameFilter(tr("VES Files (*.ves)"));
    QObject::connect( mFileDialog, SIGNAL(fileSelected(const QString &)), 
                      this, SLOT(onFileSelected(QString)) );
    QObject::connect( mFileDialog, SIGNAL(rejected()), this,
                      SLOT( onFileCancelled() ) );

    ui->tabWidget->setCurrentIndex( ui->tabWidget->addTab( mFileDialog, 
                                                           "Open File" ) );
}

void MainWindow::onFileSelected( QString fileName )
{
    std::cout << "File selected: " << fileName.toStdString() << std::endl;
    ves::conductor::NetworkLoader loader;
    loader.LoadVesFile( fileName.toStdString() );
    
    int index = ui->tabWidget->indexOf( mFileDialog );
    ui->tabWidget->removeTab( index );
    if (mFileDialog)
    {
        mFileDialog->close();
        mFileDialog = 0;
    }
}

void MainWindow::onFileCancelled()
{
    int index = ui->tabWidget->indexOf( mFileDialog );
    ui->tabWidget->removeTab( index );
    if (mFileDialog)
    {
        mFileDialog->close();
        mFileDialog = 0;
    }
}
    
