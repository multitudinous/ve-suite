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
#include "ConstraintPlugin_UIDialog.h"
#include "ui_ConstraintPlugin_UIDialog.h"

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/xplorer/command/CommandManager.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <ves/conductor/qt/UITabs.h>

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/find.hpp>

#include <QtCore/QString>
#include <QtGui/QTreeWidget>
#include <QtGui/QTreeWidgetItem>

#include <string>
#include <vector>
#include <map>
#include <fstream>

ConstraintPlugin_UIDialog::ConstraintPlugin_UIDialog(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ConstraintPlugin_UIDialog),
    m_tableCounter(0)
{
    ui->setupUi(this);

    // Connect all the logic operators to a single slot which toggle sub-blocks
    // of the Query Composition group on and off.
    /*connect( ui->m_logicOperator00, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );
    connect( ui->m_logicOperator01, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );
    connect( ui->m_logicOperator02, SIGNAL(currentIndexChanged(QString)),
             this, SLOT(m_logicOperatorS_currentIndexChanged(QString)) );

    connect( ui->m_textInput00, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput01, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput02, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));
    connect( ui->m_textInput03, SIGNAL(textChanged(QString)),
             this, SLOT(InputTextChanged(QString)));*/

    switchwire::EventManager* evm =
        switchwire::EventManager::instance();

    
    {
        std::string signalName = "ConstraintPlugin_UIDialog" +
            boost::lexical_cast<std::string>( this ) + ".ConnectToSensorServer";
        evm->RegisterSignal(
            ( &m_connectSensorSignal ),
            signalName, switchwire::EventManager::unspecified_SignalType );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintPlugin_UIDialog::on_m_sensorClientConnect_clicked()
{
    std::cout << "get ip address and send it to xplorer" << std::endl;
    std::cout << ui->m_heaterClientIP->text().toStdString() << " " 
        << ui->m_sensorClientIP->text().toStdString() << " " 
        << ui->m_sensorPort->text().toStdString() << " "
        << ui->m_heaterPort->text().toStdString() << std::endl;
    m_connectSensorSignal.signal( ui->m_sensorClientIP->text().toStdString(), ui->m_sensorPort->text().toStdString() );
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintPlugin_UIDialog::on_m_heaterClientConnect_clicked()
{
    std::cout << "get ip address and send it to xplorer" << std::endl;
    std::cout << ui->m_heaterClientIP->text().toStdString() << " " 
        << ui->m_sensorClientIP->text().toStdString() << " " 
        << ui->m_sensorPort->text().toStdString() << " "
        << ui->m_heaterPort->text().toStdString() << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintPlugin_UIDialog::on_m_testTableView_clicked()
{

}
////////////////////////////////////////////////////////////////////////////////
ConstraintPlugin_UIDialog::~ConstraintPlugin_UIDialog()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintPlugin_UIDialog::changeEvent(QEvent *e)
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
