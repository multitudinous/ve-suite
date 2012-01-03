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
#ifdef MINERVA_GIS_SUPPORT
#include <ves/conductor/qt/minerva/AddFileSystemStackedWidget.h>
#include <ves/conductor/qt/minerva/AddFileSystemWidget.h>

#include <QtGui/QFileDialog>

using namespace ves::conductor::qt::minerva;

AddFileSystemStackedWidget::AddFileSystemStackedWidget ( QWidget *parent ) : BaseClass ( parent ),
    m_fileSystemWidget ( 0x0 ),
    m_fileDialog ( 0x0 )
{
    m_fileSystemWidget = new AddFileSystemWidget ( this );
    this->addWidget ( m_fileSystemWidget );

    m_fileDialog = new QFileDialog ( this );
    m_fileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    //m_fileDialog->setAttribute( Qt::WA_DeleteOnClose );
    m_fileDialog->setFileMode( QFileDialog::ExistingFiles );

    this->addWidget ( m_fileDialog );

    QObject::connect ( m_fileSystemWidget, SIGNAL ( showFileDialog() ), this, SLOT ( showFileDialog() ) );
    QObject::connect ( m_fileDialog, SIGNAL ( accepted() ), this, SLOT ( fileDialogAccepted() ) );
    QObject::connect ( m_fileDialog, SIGNAL ( rejected() ), this, SLOT ( fileDialogRejected() ) );

    QObject::connect ( m_fileDialog, SIGNAL ( filesSelected( const QStringList& ) ), m_fileSystemWidget, SLOT ( onFilesSelected( const QStringList& ) ) );
    //QObject::connect ( m_fileDialog, SIGNAL ( fileSelected( const QString& ) ), m_fileSystemWidget, SLOT ( onFileSelected( const QString& ) ) );
}

AddFileSystemStackedWidget::~AddFileSystemStackedWidget()
{
    this->removeWidget ( m_fileSystemWidget );
    delete m_fileSystemWidget;

    this->removeWidget ( m_fileDialog );
    delete m_fileDialog;
}

void AddFileSystemStackedWidget::showFileDialog()
{
    this->setCurrentIndex ( 1 );
}

void AddFileSystemStackedWidget::fileDialogAccepted()
{
    this->setCurrentIndex ( 0 );
}

void AddFileSystemStackedWidget::fileDialogRejected()
{
    this->setCurrentIndex ( 0 );
}

void AddFileSystemStackedWidget::AddLayersToFeature ( Minerva::Core::Data::Container* container )
{
    m_fileSystemWidget->AddLayersToFeature ( container );
}

#endif
