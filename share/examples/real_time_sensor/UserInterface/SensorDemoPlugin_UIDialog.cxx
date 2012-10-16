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
#include "ui_SensorDemoPlugin_UIDialog.h"

#include <QtCore/QString>
#include <QtGui/QTreeWidget>
#include <QtGui/QTreeWidgetItem>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/xplorer/command/CommandManager.h>

#include <vpr/vpr.h>
#include <vpr/System.h>
#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/TimeoutException.h>
#include <vpr/Util/Interval.h>

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/find.hpp>

#include <string>
#include <vector>
#include <map>
#include <fstream>

#include <ves/conductor/qt/UITabs.h>

#include "SensorDemoPlugin_UIDialog.h"

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <Poco/Data/SQLite/SQLite.h>
#include <Poco/Data/SQLite/Connector.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/Statement.h>
#include <Poco/Data/RecordSet.h>

SensorDemoPlugin_UIDialog::SensorDemoPlugin_UIDialog(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::SensorDemoPlugin_UIDialog),
    m_tableCounter(0)
{
    ui->setupUi(this);

    switchwire::EventManager* evm =
        switchwire::EventManager::instance();

    
    {
        std::string signalName = "SensorDemoPlugin_UIDialog" +
            boost::lexical_cast<std::string>( this ) + ".ConnectToSensorServer";
        evm->RegisterSignal(
            ( &m_connectSensorSignal ),
            signalName, switchwire::EventManager::unspecified_SignalType );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPlugin_UIDialog::on_m_sensorClientConnect_clicked()
{
    std::cout << "get ip address and send it to xplorer" << std::endl;
    std::cout << ui->m_sensorClientIP->text().toStdString() << " " 
        << ui->m_sensorPort->text().toStdString() << std::endl;
    m_connectSensorSignal.signal( ui->m_sensorClientIP->text().toStdString(),
                          ui->m_sensorPort->text().toStdString() );
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPlugin_UIDialog::on_m_heaterClientConnect_clicked()
{
    std::cout << "get ip address to send to the heater" << std::endl;
    std::cout << ui->m_heaterClientIP->text().toStdString() << " " 
        << ui->m_heaterPort->text().toStdString() << std::endl;
    LaunchServerThread( ui->m_heaterClientIP->text().toStdString(), 
                       ui->m_heaterPort->text().toStdString() );
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPlugin_UIDialog::on_m_testTableView_clicked()
{
    QTreeWidget* queryResults = new QTreeWidget( 0 );
    //QTreeWidget* queryResults = new QTreeWidget( ui->m_sensorData );
    
    queryResults->setColumnCount( 2 );
    
    std::string partNumber;
    std::string partNumberHeader;
    std::string partText;
    
    // Get header names
    QStringList headers;
    headers.append( QString::fromStdString( "Sensor Number" ) );
    headers.append( QString::fromStdString( "Sensor Type" ) );

    /*if( more )
    {
        for (std::size_t col = 0; col < cols; ++col)
        {
            partNumberHeader = rs.columnName( col );
            headers.append( QString::fromStdString( partNumberHeader ));
        }
    }*/
    
    queryResults->setHeaderLabels( headers );
    
    // Keep everything to the right of "WHERE " in the query to use as the tab
    // title
    QString title = QString::fromStdString( "Sensor Information" );
    
    ves::conductor::UITabs::instance()->
    ActivateTab( ves::conductor::UITabs::instance()->
                AddTab( queryResults, title.toStdString() ) );
    
    /*while (more)
    {
        QStringList recordData;
        for (std::size_t col = 0; col < cols; ++col)
        {
            recordData.append( QString::fromStdString( rs[col].convert<std::string>() ) );
        }
        QTreeWidgetItem* item = new QTreeWidgetItem( queryResults, recordData );
        
        more = rs.moveNext();
    }*/
    
    ///
    /*{
        QStringList recordData;
        recordData.append( QString::fromStdString( "1" ) );
        recordData.append( QString::fromStdString( "Thermocouple" ) );
        QTreeWidgetItem* item = new QTreeWidgetItem( queryResults, recordData );
    }
    ///
    {
        QStringList recordData;
        recordData.append( QString::fromStdString( "2" ) );
        recordData.append( QString::fromStdString( "Super Thermocouple" ) );
        QTreeWidgetItem* item = new QTreeWidgetItem( queryResults, recordData );
    }*/
    
    queryResults->setSortingEnabled( true );
}
////////////////////////////////////////////////////////////////////////////////
SensorDemoPlugin_UIDialog::~SensorDemoPlugin_UIDialog()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPlugin_UIDialog::changeEvent(QEvent *e)
{
    QWidget::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPlugin_UIDialog::StripCharacters( std::string& data, const std::string& character )
{
    for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( character, index );
        if ( index != std::string::npos )
        {
            data.replace( index, 1, "\n" );
            //data.erase( index, 1 );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPlugin_UIDialog::LaunchServerThread( std::string const& ipAddress, std::string const& portNumber )
{
    int app_status;
    
    vpr::InetAddr local;
    vpr::Uint16 port = boost::lexical_cast< unsigned short >( portNumber );
    local.setAddress( ipAddress, port );

    // Create an acceptor socket that listens on port.
    vpr::SocketStream sock( local, vpr::InetAddr::AnyAddr );
    
    try
    {
        // Open in server mode.
        sock.openServer();
        sock.setReuseAddr(true);
        
        // Loop forever handling all clients serially.
        //while ( true )
        {
            // Wait for an incoming connection.
            try
            {
                vpr::SocketStream client_sock;
                sock.accept( client_sock, vpr::Interval(60, vpr::Interval::Sec));
                
                while( true )
                {
                    // Using the new socket, send the buffer to the client and close
                    // the socket.
                    if( m_sendData )
                    {
                        vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
                        client_sock.write( m_dataBuffer, m_dataBuffer.length() );
                        m_sendData = false;
                    }
                    else
                    {
                        vpr::System::msleep( 300 );
                    }
                }
                client_sock.close();
            }
            catch( vpr::TimeoutException& )
            {
                std::cerr << "No connections within timeout period!\n";
                //break;
            }
            catch( vpr::SocketException& ex )
            {
                std::cerr << "Caught a socket exception:\n" << ex.what()
                    << std::endl;
            }
        }
        
        app_status = EXIT_SUCCESS;
    }
    catch( vpr::IOException& ex )
    {
        std::cerr << "Caught an I/O exception:\n" << ex.what() << std::endl;
        app_status = EXIT_FAILURE;
    }
    
    return;    
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPlugin_UIDialog::on_m_heaterSpinBox_valueChanged( double d )
{
    vpr::Guard<vpr::Mutex> val_guard( m_valueLock );
    ///double heaterSetting = m_heaterSpinBox->getValue();
    m_dataBuffer = boost::lexical_cast< std::string >( d );
    m_sendData = true;
    ui->m_heaterSlider->setSliderPosition( int( d * 100 ) );
}
////////////////////////////////////////////////////////////////////////////////
void SensorDemoPlugin_UIDialog::on_m_heaterSlider_sliderReleased()
{
    ui->m_heaterSpinBox->setValue( ui->m_heaterSlider->value() * 0.01f );
}
////////////////////////////////////////////////////////////////////////////////
