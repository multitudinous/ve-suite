/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include "LauncherMainWindow.h"
#include "ui_LauncherMainWindow.h"
#include <QtGui/QDesktopWidget>
#include <QtGui/QFileDialog>
#include <QtGui/QMessageBox>
#include <QtCore/QSettings>
#include <QtCore/QTextStream>
#include <QtCore/QFile>

LauncherMainWindow::LauncherMainWindow( QWidget* parent ) :
    QMainWindow( parent ),
    ui( new Ui::LauncherMainWindow )
{
    ui->setupUi( this );
//    QDesktopWidget* dw = QApplication::desktop();
//    const QRect geom = dw->screenGeometry( dw->screenNumber( this ) );
//    ui->m_width->setValue( geom.width() );
//    ui->m_height->setValue( geom.height() );

    // Parse the configurations file to put into m_configuration



    QSettings settings( "VESuite.org", "VELauncher" );

    bool notFirstRun = settings.value( "launcher/notFirstRun" ).toBool();

    if( notFirstRun )
    {
        ui->m_desktop->setChecked( settings.value( "xplorer/desktop" ).toBool() );
        ui->m_cluster->setChecked( settings.value( "xplorer/cluster" ).toBool() );
        ui->m_desktopClusterControl->setChecked( settings.value( "xplorer/desktopclustercontrol" ).toBool() );

        QString configFile = settings.value( "xplorer/clusterconfigfile" ).toString();
        ui->m_configurationFilename->setText( configFile );
        if( !configFile.isEmpty() )
        {
            ParseConfigFile( configFile );
            ui->m_configuration->setCurrentIndex(
               ui->m_configuration->findText(
                  settings.value( "xplorer/clusternamedconfig" ).toString() ) );
        }

        ui->m_vesLogLevel->setCurrentIndex( settings.value( "xplorer/logLevel" ).toInt() );
        ui->m_jugglerLogLevel->setCurrentIndex( settings.value( "juggler/logLevel" ).toInt() );
        ui->m_osgLogLevel->setCurrentIndex( settings.value( "osg/logLevel" ).toInt() );

        ui->m_showStdout->setChecked( settings.value( "xplorer/showstdout" ).toBool() );

        int width = settings.value( "launcher/width" ).toInt();
        int height = settings.value( "launcher/height" ).toInt();

        if( width != 0 && height != 0 )
        {
            this->resize( settings.value( "launcher/width" ).toInt(),
                          settings.value( "launcher/height" ).toInt() );
        }
        this->move( settings.value( "launcher/xposition" ).toInt(),
                    settings.value( "launcher/yposition" ).toInt() );
    }
}

LauncherMainWindow::~LauncherMainWindow()
{
    delete ui;
}

void LauncherMainWindow::changeEvent( QEvent* e )
{
    QMainWindow::changeEvent( e );
    switch( e->type() )
    {
    case QEvent::LanguageChange:
        ui->retranslateUi( this );
        break;
    default:
        break;
    }
}

void LauncherMainWindow::ParseConfigFile(QString filename)
{
    QFile file( filename );
    if ( !file.open(QIODevice::ReadOnly | QIODevice::Text) )
    {
        return;
    }

    QTextStream in(&file);
    while( !in.atEnd() )
    {
        QString line = in.readLine();
        if( line.indexOf("vrjconfig") == 0 )
        {
            QStringList tokens = line.split(" ");
            if( tokens.size() == 3 )
            {
                ui->m_configuration->addItem( tokens.at( 1 ) );
            }
        }
    }
}

void LauncherMainWindow::m_process_error( QProcess::ProcessError error )
{
    QMessageBox message;
    message.setText("Error starting VE-Suite: Please ensure the directory containing ves_xplorer is in your environment's PATH.");
    message.exec();
    this->close();
}

void LauncherMainWindow::m_process_started()
{
    if( ui->m_desktop->isChecked() )
    {
        this->close();
    }
}

void LauncherMainWindow::on_m_configurationButton_clicked()
{
    QString fileName = QFileDialog::getOpenFileName( this, tr( "Select configuration file" ),
                       "",
                       tr( "Cluster Configuration (*.cconf);;*.* (*.*)" ) );

    if( !fileName.isEmpty() )
    {
        ui->m_configurationFilename->setText( fileName );
    }
    ParseConfigFile( fileName );
}

void LauncherMainWindow::on_m_killButton_clicked()
{
    StartProcess( "kill" );
}

void LauncherMainWindow::on_m_shutdownButton_clicked()
{
    StartProcess( "kill" );
}

void LauncherMainWindow::on_m_restartButton_clicked()
{
    StartProcess( "kill" );
    StartProcess( "launch" );
}

void LauncherMainWindow::on_m_launchButton_clicked()
{
    StartProcess( "launch" );
}

void LauncherMainWindow::StartProcess( const QString& action )
{
    QString program;
    QStringList arguments;
    if( ui->m_desktop->isChecked() )
    {
        program = "launch-ves_xplorer-desktop.sh";
    }
    else if( ui->m_cluster->isChecked() )
    {
        program = "ves-cluster-control.sh";
        arguments << "-c" << ui->m_configurationFilename->text()
                  << "-n" << ui->m_configuration->currentText()
                  << "-a" << action;
    }

    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    env.insert( "VES_LOG_LEVEL", ui->m_vesLogLevel->currentText() );
    QString num;
    env.insert( "VPR_DEBUG_NFY_LEVEL", num.setNum( ui->m_jugglerLogLevel->currentIndex() ) );
    env.insert( "OSG_NOTIFY_LEVEL", ui->m_osgLogLevel->currentText() );
    m_process = new QProcess( 0 );
    connect( m_process, SIGNAL(started()), this, SLOT(m_process_started()) );
    connect( m_process, SIGNAL(error(QProcess::ProcessError)), this, SLOT(m_process_error(QProcess::ProcessError)) );
    m_process->setProcessEnvironment( env );
    m_process->start( program, arguments );

    // Open a textedit window and use it to display contents of stdoutput from
    // xplorer

    if( ui->m_showStdout->isChecked() )
    {
        m_stdout = new QTextEdit( 0 );
        m_stdout->resize( 640, 360 );
        m_stdout->show();
        m_stdout->setWindowTitle( "VE-Suite stdoutput" );
        connect( m_process, SIGNAL( readyReadStandardOutput() ), this, SLOT( onReadyReadStandardOutput() ) );
    }



    QSettings settings( "VESuite.org", "VELauncher" );

    settings.setValue( "xplorer/desktop", ui->m_desktop->isChecked() );
    settings.setValue( "xplorer/cluster", ui->m_cluster->isChecked() );
    settings.setValue( "xplorer/desktopclustercontrol", ui->m_desktopClusterControl->isChecked() );
    settings.setValue( "xplorer/clusterconfigfile", ui->m_configurationFilename->text() );
    settings.setValue( "xplorer/clusternamedconfig", ui->m_configuration->currentText() );
    settings.setValue( "xplorer/logLevel", ui->m_vesLogLevel->currentIndex() );
    settings.setValue( "juggler/logLevel", ui->m_jugglerLogLevel->currentIndex() );
    settings.setValue( "osg/logLevel", ui->m_osgLogLevel->currentIndex() );
    settings.setValue( "xplorer/showstdout", ui->m_showStdout->isChecked() );
    settings.setValue( "launcher/notFirstRun", true );
    settings.setValue( "launcher/width", this->width() );
    settings.setValue( "launcher/height", this->height() );
    settings.setValue( "launcher/xposition", this->x() );
    settings.setValue( "launcher/yposition", this->y() );
}

void LauncherMainWindow::onReadyReadStandardOutput()
{
    QByteArray data = m_process->readAllStandardOutput();
    QString stringData( data );
    m_stdout->append( stringData );
}
