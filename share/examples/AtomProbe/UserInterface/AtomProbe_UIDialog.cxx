/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
 * Date modified: $Date: 2011-10-07 16:20:34 -0500 (Fri, 07 Oct 2011) $
 * Version:       $Rev: 16404 $
 * Author:        $Author: tjordan $
 * Id:            $Id: AtomProbe_UIDialog.cxx 16404 2011-10-07 21:20:34Z tjordan $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
//#include <ves/conductor/qt/AtomProbe/AtomProbe_UIDialog.h>
//#include <ves/conductor/qt/AtomProbe/ui_AtomProbe_UIDialog.h>

#include "AtomProbe_UIDialog.h"
#include "ui_AtomProbe_UIDialog.h"
#include <ves/xplorer/command/CommandManager.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/find.hpp>
//#include <QString>
#include <QtGui/QColorDialog>
#include <QtGui/QFileDialog>
#include <QtGui/QInputDialog>
AtomProbe_UIDialog::AtomProbe_UIDialog(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::AtomProbe_UIDialog)
{
    ui->setupUi(this);
	connect( ui->colorizeSpinbox, SIGNAL(valueChanged(int)), this, SLOT(setData(int)));
	connect( ui->minSpinbox, SIGNAL(valueChanged(double)), this, SLOT(setBottom(double)));
	connect( ui->maxSpinbox, SIGNAL(valueChanged(double)), this, SLOT(setTop(double)));
	connect(this, SIGNAL(colorizeHistogramSignal()), this, SLOT(qtUpdateColorizeHistogram()));
	connect(this, SIGNAL(maskHistogramSignal()), this, SLOT(qtUpdateMaskHistogram()));
	
	connect( ui->maskSpinbox, SIGNAL(valueChanged(int)), this, SLOT(setMaskData(int)));
	connect( ui->maskMinSpinbox, SIGNAL(valueChanged(double)), this, SLOT(setMaskBottom(double)));
	connect( ui->maskMaxSpinbox, SIGNAL(valueChanged(double)), this, SLOT(setMaskTop(double)));
	
	
	connect( ui->hit0, SIGNAL(stateChanged(int)), this, SLOT(setHit0Enabled(int)));
	connect(ui->hit0Color, SIGNAL(clicked()), this, SLOT(setHit0Color()));
	
	
	connect( ui->hit1, SIGNAL(stateChanged(int)), this, SLOT(setHit1Enabled(int)));
	connect(ui->hit1Color, SIGNAL(clicked()), this, SLOT(setHit1Color()));
	
	connect( ui->hit2, SIGNAL(stateChanged(int)), this, SLOT(setHit2Enabled(int)));
	connect(ui->hit2Color, SIGNAL(clicked()), this, SLOT(setHit2Color()));
	
	connect( ui->hit3, SIGNAL(stateChanged(int)), this, SLOT(setHit3Enabled(int)));
	connect(ui->hit3Color, SIGNAL(clicked()), this, SLOT(setHit3Color()));
	connect(ui->pulseSlider, SIGNAL(sliderMoved(int)), this, SLOT(setLastTime(int)));
	
    switchwire::EventManager* evm =
    switchwire::EventManager::instance();

    
    {
        std::string signalName = "AtomProbe_UIDialog" +
		boost::lexical_cast<std::string>( this ) + ".SelectColorizeDataSignal";
        evm->RegisterSignal(
                            ( &m_SelectColorizeDataSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }
	
	{
        std::string signalName = "AtomProbe_UIDialog" +
		boost::lexical_cast<std::string>( this ) + ".SetMaskVariableSignal";
        evm->RegisterSignal(
                            ( &m_SetMaskVariableSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }
	
	{
        std::string signalName = "AtomProbe_UIDialog" +
		boost::lexical_cast<std::string>( this ) + ".SetMaskBottomSignal";
        evm->RegisterSignal(
                            ( &m_SetMaskBottomSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }
	
	{
        std::string signalName = "AtomProbe_UIDialog" +
		boost::lexical_cast<std::string>( this ) + ".SetMaskTopSignal";
        evm->RegisterSignal(
                            ( &m_SetMaskTopSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }
	
	{
        std::string signalName = "AtomProbe_UIDialog" +
		boost::lexical_cast<std::string>( this ) + ".SetHitColorizeSignal";
        evm->RegisterSignal(
                            ( &m_SetHitColorizeSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }
	
	
	{
        std::string signalName = "AtomProbe_UIDialog" +
		boost::lexical_cast<std::string>( this ) + ".SetLastTimeSignal";
        evm->RegisterSignal(
                            ( &m_SetLastTimeSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }
	
	
	colorizeHistogramScene = new QGraphicsScene();
	for(int i = 0; i < 40; i++) {
		colorizeHistogramScene->addRect(i,-i*3,1,i*3);
	}
	ui->HistogramColorize->setScene(colorizeHistogramScene);
	ui->HistogramColorize->fitInView(0,-20,20,20);
	
	maskHistogramScene = new QGraphicsScene();
	for(int i = 0; i < 40; i++) {
		maskHistogramScene->addRect(i,-i*3,1,i*3);
	}
	ui->HistogramMask->setScene(maskHistogramScene);
	ui->HistogramMask->fitInView(0,-20,20,20);
	
	
	CONNECTSIGNALS_4("%HistogramDataSignal",void (std::vector< int > const&,std::vector< int > const&,std::vector< int > const&, std::vector< double > const&),
					&AtomProbe_UIDialog::updateColorizeHistogram,
					m_connections, any_SignalType, normal_Priority);
	
	CONNECTSIGNALS_1("%MaskHistogramDataSignal",void ( std::vector< double > const&),
					 &AtomProbe_UIDialog::updateMaskHistogram,
					 m_connections, any_SignalType, normal_Priority);
	
	CONNECTSIGNAL_1( "%SetNumPulses",
                    void( double const& ),
                    &AtomProbe_UIDialog::setNumPulses,
                    m_connections, normal_Priority );
	
	
	
	labelText.push_back(QString("X Coordinate"));
	labelText.push_back(QString("Y Coordinate"));
	labelText.push_back(QString("Z Coordinate"));
	labelText.push_back(QString("amu"));
	labelText.push_back(QString("TOF"));
	labelText.push_back(QString("Vdc"));
	labelText.push_back(QString("Vp"));
	labelText.push_back(QString("X Detector Coord"));
	labelText.push_back(QString("Y Detector Coord"));
	labelText.push_back(QString("Delta P"));
	labelText.push_back(QString("Nm"));
	
	
	
	
	
	for(int i = 0; i < 4; i++)
	{
		rrHit.push_back(0);
		ggHit.push_back(0);
		bbHit.push_back(0);
		hitEnabled.push_back(0);
	}
	
	
}

AtomProbe_UIDialog::~AtomProbe_UIDialog()
{
    delete ui;
	
}
void AtomProbe_UIDialog::updateColorizeHistogram(std::vector<int> r, std::vector<int> g,std::vector<int> b, std::vector<double> values) {
	curValues = values;
	rr=r;
	gg=g;
	bb=b;
	colorizeHistogramSignal();
	

}

void AtomProbe_UIDialog::updateMaskHistogram(std::vector<double> values) {
	curMaskValues = values;
	maskHistogramSignal();
}

void AtomProbe_UIDialog::qtUpdateColorizeHistogram() {
	std::cout << "made it into qt fucntion" << std::endl;
	
	QList<QGraphicsItem *> list = colorizeHistogramScene->items();
	QList<QGraphicsItem *>::iterator j;
	//QBrush *brush = new QBrush(QColor(0,255,0));
	//QPen *pen = new QPen(QColor(0,255,0));
	for(j = list.begin(); j!= list.end(); j++) {
		colorizeHistogramScene->removeItem(*j);
	}
	for(int i = 0; i < curValues.size(); i++) {
		colorizeHistogramScene->addRect(i,-6*curValues[i],1,curValues[i]*6,QPen(),QBrush(QColor(rr[i],gg[i],bb[i])));
		
	}
}

void AtomProbe_UIDialog::changeEvent(QEvent *e)
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

void AtomProbe_UIDialog::enableLattice() {
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

void AtomProbe_UIDialog::enableDislocations() {
	
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

void AtomProbe_UIDialog::toggleAnimation() {
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

void AtomProbe_UIDialog::disableAll() {
	
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


void AtomProbe_UIDialog::setFrame(int frame) {
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

void AtomProbe_UIDialog::setData(int data) {
	char suffix[4];
	sprintf(suffix, "%d",data);
	std::string commanddata= suffix;
	std::cout << data << " " << commanddata << std::endl;
	
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "SET_DATA", commanddata );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );
	
	
    m_SelectColorizeDataSignal.signal(data);
	
	ui->colorizeLabel->setText(labelText[data]);
	
}

void AtomProbe_UIDialog::setBottom(double bottom) {
	char suffix[12];
	sprintf(suffix, "%f",bottom);
	std::string commanddata= suffix;
	std::cout << bottom << " " << commanddata << std::endl;
	
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "SET_BOTTOM", commanddata );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );
	
	
	
	
}

void AtomProbe_UIDialog::setTop(double top) {
	char suffix[12];
	sprintf(suffix, "%f",top);
	std::string commanddata= suffix;
	std::cout << top << " " << commanddata << std::endl;
	
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "SET_TOP", commanddata );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );
	
	
	
	
}


void AtomProbe_UIDialog::setMaskData(int data) {
	
	
	//QFileDialog *m_fileDialog =  new QFileDialog(0);
	//QColorDialog* colorDialog = new QColorDialog(QColor(0,0,50), 0);
	//QInputDialog* inputDialog = new QInputDialog();
	
	//ves::conductor::UITabs* tabs = ves::conductor::UITabs::instance();
	
    /*if( m_fileDialog )
	 {
	 tabs->ActivateTab( tabs->AddTab( m_fileDialog, "Select File" ) );
	 return;
	 }*/
	
    //m_fileDialog = new QFileDialog( 0 );
	//m_fileDialog->setOptions(QFileDialog::DontUseNativeDialog);
	//colorDialog->setAttribute( Qt::WA_DeleteOnClose );
    //colorDialog->setOptions( QColorDialog::DontUseNativeDialog );
    //colorDialog->setAttribute( Qt::WA_DeleteOnClose );
    //colorDialog->setFileMode( QFileDialog::ExistingFile );
	
    /*QStringList filters;
	 filters << "SQLite DBs (*.db)" << "CSV files (*.csv)" << "All Files (*.*)";
	 m_fileDialog->setNameFilters( filters );
	 
	 connect( m_fileDialog, SIGNAL(fileSelected(const QString &)),
	 this, SLOT(onFileSelected(const QString&)) );
	 connect( m_fileDialog, SIGNAL(rejected()), this,
	 SLOT( onFileCancelled() ) );*/
	//colorDialog->setVisible(true);
    //tabs->ActivateTab( tabs->AddTab( colorDialog, "Select Color" ) );
	//colorDialog->show();
	//colorDialog.open();
	//colorDialog->open();
	/*char suffix[4];
	sprintf(suffix, "%d",data);
	std::string commanddata= suffix;
	std::cout << data << " " << commanddata << std::endl;
	
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "SET_MASKDATA", commanddata );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );*/
	std::cout <<"set mask var qt" << std::endl;
	ui->maskLabel->setText(labelText[data]);
    m_SetMaskVariableSignal.signal(data);
	
	//QColor woot = QColorDialog::getColor();
	//std::cout << woot.red() << " " << woot.green() << " " << woot.blue() << std::endl;
	//updateMaskHistogram();
	
	
	
	
	
}

void AtomProbe_UIDialog::setMaskBottom(double bottom) {
	/*char suffix[12];
	sprintf(suffix, "%f",bottom);
	std::string commanddata= suffix;
	std::cout << bottom << " " << commanddata << std::endl;
	
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "SET_MASKBOTTOM", commanddata );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );*/
	std::cout <<"set mask bottom qt" << std::endl;
    m_SetMaskBottomSignal.signal(bottom);
	//updateMaskHistogram();
	
	
	
}

void AtomProbe_UIDialog::setMaskTop(double top) {
	/*char suffix[12];
	sprintf(suffix, "%f",top);
	std::string commanddata= suffix;
	std::cout << top << " " << commanddata << std::endl;
	
	
	std::cout << "command sent" << std::endl;
	ves::open::xml::DataValuePairSharedPtr enableLattice(
														 new ves::open::xml::DataValuePair() );
	enableLattice->SetData( "SET_MASKTOP", commanddata );
	ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	command->AddDataValuePair( enableLattice );
	std::string mCommandName = "MULTISCALE_COMMAND";
	command->SetCommandName( mCommandName );
	ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );*/
	std::cout <<"set mask top qt" << std::endl;
    m_SetMaskTopSignal.signal(top);
	//updateMaskHistogram();
	
	
	
	
}

void AtomProbe_UIDialog::setLastTime(int pulse) {
	/*char suffix[12];
	 sprintf(suffix, "%f",top);
	 std::string commanddata= suffix;
	 std::cout << top << " " << commanddata << std::endl;
	 
	 
	 std::cout << "command sent" << std::endl;
	 ves::open::xml::DataValuePairSharedPtr enableLattice(
	 new ves::open::xml::DataValuePair() );
	 enableLattice->SetData( "SET_MASKTOP", commanddata );
	 ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
	 command->AddDataValuePair( enableLattice );
	 std::string mCommandName = "MULTISCALE_COMMAND";
	 command->SetCommandName( mCommandName );
	 ves::xplorer::command::CommandManager::instance()->AddXMLCommand( command );*/
	std::cout <<"set last time qt" << std::endl;
    m_SetLastTimeSignal.signal(pulse);
	//updateMaskHistogram();
	
	
	
	
}


void AtomProbe_UIDialog::qtUpdateMaskHistogram() {
	QList<QGraphicsItem *> list = maskHistogramScene->items();
	QList<QGraphicsItem *>::iterator j;
	//QBrush *brush = new QBrush(QColor(0,255,0));
	//QPen *pen = new QPen(QColor(0,255,0));
	for(j = list.begin(); j!= list.end(); j++) {
		maskHistogramScene->removeItem(*j);
	}
	for(int i = 0; i < curMaskValues.size(); i++) {
		bool masked = false;
		if(((float)i)*.025 < ui->maskMinSpinbox->value())
			masked = true;
		if(((float)i)*.025 > ui->maskMaxSpinbox->value())
			masked = true;
		if(!masked)
			maskHistogramScene->addRect(i,-6*curMaskValues[i],1,curMaskValues[i]*6,QPen(),QBrush(QColor(0,0,0)));
		else
			maskHistogramScene->addRect(i,-6*curMaskValues[i],1,curMaskValues[i]*6);
		std::cout << i << ":" <<curMaskValues[i] << std::endl;
	}
	
}

void AtomProbe_UIDialog::setHit0Enabled(int state) {
	//std::cout << "state changed" << std::endl;
	hitEnabled[0] = state;
    m_SetHitColorizeSignal.signal(rrHit, ggHit,bbHit,hitEnabled);
	
}

void AtomProbe_UIDialog::setHit1Enabled(int state) {
	//std::cout << "state changed" << std::endl;
	hitEnabled[1] = state;
    m_SetHitColorizeSignal.signal(rrHit, ggHit,bbHit,hitEnabled);
	
}

void AtomProbe_UIDialog::setHit2Enabled(int state) {
	//std::cout << "state changed" << std::endl;
	hitEnabled[2] = state;
    m_SetHitColorizeSignal.signal(rrHit, ggHit,bbHit,hitEnabled);
	
}

void AtomProbe_UIDialog::setHit3Enabled(int state) {
	//std::cout << "state changed" << std::endl;
	hitEnabled[3] = state;
    m_SetHitColorizeSignal.signal(rrHit, ggHit,bbHit,hitEnabled);
	
}

void AtomProbe_UIDialog::setHit0Color() {
	QColor woot = QColorDialog::getColor();
	std::cout << woot.red() << " " << woot.green() << " " << woot.blue() << std::endl;
	rrHit[0] = woot.red();
	ggHit[0] = woot.green();
	bbHit[0] = woot.blue();
	
	
	QPalette pal = ui->hit0ColorLabel->palette();
	pal.setColor(ui->hit0ColorLabel->backgroundRole(), woot);
	ui->hit0ColorLabel->setPalette(pal);
	ui->hit0ColorLabel->setAutoFillBackground(true);
    m_SetHitColorizeSignal.signal(rrHit, ggHit,bbHit,hitEnabled);
	
}

void AtomProbe_UIDialog::setHit1Color() {
	QColor woot = QColorDialog::getColor();
	std::cout << woot.red() << " " << woot.green() << " " << woot.blue() << std::endl;
	rrHit[1] = woot.red();
	ggHit[1] = woot.green();
	bbHit[1] = woot.blue();
	
	
	QPalette pal = ui->hit1ColorLabel->palette();
	pal.setColor(ui->hit1ColorLabel->backgroundRole(), woot);
	ui->hit1ColorLabel->setPalette(pal);
	ui->hit1ColorLabel->setAutoFillBackground(true);
    m_SetHitColorizeSignal.signal(rrHit, ggHit,bbHit,hitEnabled);
	
}

void AtomProbe_UIDialog::setHit2Color() {
	QColor woot = QColorDialog::getColor();
	std::cout << woot.red() << " " << woot.green() << " " << woot.blue() << std::endl;
	rrHit[2] = woot.red();
	ggHit[2] = woot.green();
	bbHit[2] = woot.blue();
	
	
	QPalette pal = ui->hit2ColorLabel->palette();
	pal.setColor(ui->hit2ColorLabel->backgroundRole(), woot);
	ui->hit2ColorLabel->setPalette(pal);
	ui->hit2ColorLabel->setAutoFillBackground(true);
    m_SetHitColorizeSignal.signal(rrHit, ggHit,bbHit,hitEnabled);
	
}

void AtomProbe_UIDialog::setHit3Color() {
	QColor woot = QColorDialog::getColor();
	std::cout << woot.red() << " " << woot.green() << " " << woot.blue() << std::endl;
	rrHit[3] = woot.red();
	ggHit[3] = woot.green();
	bbHit[3] = woot.blue();
	
	
	QPalette pal = ui->hit3ColorLabel->palette();
	pal.setColor(ui->hit3ColorLabel->backgroundRole(), woot);
	ui->hit3ColorLabel->setPalette(pal);
	ui->hit3ColorLabel->setAutoFillBackground(true);
    m_SetHitColorizeSignal.signal(rrHit, ggHit,bbHit,hitEnabled);
	
}


void AtomProbe_UIDialog::qtUpdateNumPulses() {
}


