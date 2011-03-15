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
 * Date modified: $Date: 2011-01-03 22:17:45 -0600 (Mon, 03 Jan 2011) $
 * Version:       $Rev: 15339 $
 * Author:        $Author: mccdo $
 * Id:            $Id: LayersTree.h 15339 2011-01-04 04:17:45Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifdef MINERVA_GIS_SUPPORT
#include <ves/conductor/qt/minerva/AddFileSystemStackedWidget.h>
#include <ves/conductor/qt/minerva/AddFileSystemWidget.h>

#include <QtGui/QFileDialog>

using namespace ves::conductor::qt::minerva;

AddFileSystemStackedWidget::AddFileSystemStackedWidget ( QWidget *parent ) : BaseClass ( parent ),
    mFileSystemWidget ( 0x0 ),
    mFileDialog ( 0x0 )
{
    mFileSystemWidget = new AddFileSystemWidget ( this );
    this->addWidget ( mFileSystemWidget );

    mFileDialog = new QFileDialog ( this );
    mFileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    mFileDialog->setAttribute( Qt::WA_DeleteOnClose );
    mFileDialog->setFileMode( QFileDialog::ExistingFiles );

    this->addWidget ( mFileDialog );

    QObject::connect ( mFileSystemWidget, SIGNAL ( showFileDialog() ), this, SLOT ( showFileDialog() ) );
    QObject::connect ( mFileDialog, SIGNAL ( accepted() ), this, SLOT ( fileDialogAccepted() ) );
    QObject::connect ( mFileDialog, SIGNAL ( rejected() ), this, SLOT ( fileDialogRejected() ) );

    QObject::connect ( mFileDialog, SIGNAL ( filesSelected( const QStringList& ) ), mFileSystemWidget, SLOT ( onFilesSelected( const QStringList& ) ) );
    //QObject::connect ( mFileDialog, SIGNAL ( fileSelected( const QString& ) ), mFileSystemWidget, SLOT ( onFileSelected( const QString& ) ) );
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

#endif
