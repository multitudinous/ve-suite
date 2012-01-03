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
//#include <ves/conductor/qt/Multiscale2/Multiscale2_UIDialog.h>
//#include <ves/conductor/qt/Multiscale2/ui_Multiscale2_UIDialog.h>

#include "Multiscale2_UIDialog.h"
#include "ui_Multiscale2_UIDialog.h"

Multiscale2_UIDialog::Multiscale2_UIDialog(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Multiscale2_UIDialog)
{
    ui->setupUi(this);
	connect( ui->enableLattice, SIGNAL(clicked()), this, SLOT(enableLattice()));
	connect( ui->enableDislocations, SIGNAL(clicked()), this, SLOT(enableDislocations()));
	
	connect( ui->disableAll, SIGNAL(clicked()), this, SLOT(disableAll()));
	
	connect( ui->latticeFrame, SIGNAL(sliderMoved(int)), this, SLOT(setFrame(int)));
	connect( ui->dislocationsFrame, SIGNAL(sliderMoved(int)), this, SLOT(setFrame(int)));
	connect( ui->toggleAnimation, SIGNAL(clicked()),this, SLOT(toggleAnimation()));
	
}

Multiscale2_UIDialog::~Multiscale2_UIDialog()
{
    delete ui;
	
}

void Multiscale2_UIDialog::changeEvent(QEvent *e)
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

void Multiscale2_UIDialog::enableLattice() {
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
																  new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "ENABLE_LATTICE", "TRUE" );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );

}

void Multiscale2_UIDialog::enableDislocations() {
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "ENABLE_DISLOCATIONS", "TRUE" );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );
	
}

void Multiscale2_UIDialog::toggleAnimation() {
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "TOGGLE_ANIMATION", "TRUE" );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );
	
}

void Multiscale2_UIDialog::disableAll() {
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "DISABLE_ALL", "TRUE" );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );
}


void Multiscale2_UIDialog::setFrame(int frame) {
	char suffix[4];
	sprintf(suffix, "%d",frame);
	std::string commanddata= suffix;
	std::cout << frame << " " << commanddata << std::endl;
	
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "SET_FRAME", commanddata );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );
	
	
	

}
