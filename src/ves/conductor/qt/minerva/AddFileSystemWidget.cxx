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
#include <ves/conductor/qt/minerva/AddFileSystemWidget.h>

#include <ves/conductor/qt/minerva/ui_AddFileSystemWidget.h>

using namespace ves::conductor::qt::minerva;

AddFileSystemWidget::AddFileSystemWidget ( QWidget *parent ) : BaseClass ( parent ),
  _ui ( new Ui::AddFileSystemWidget )
{
    _ui->setupUi ( this );
}

void AddFileSystemWidget::on_addFilesButton_clicked()
{
    emit showFileDialog();
}

void AddFileSystemWidget::on_removeSelectedFilesButton_clicked()
{
    typedef QList<QListWidgetItem *> Items;
    Items items ( _ui->listWidget->selectedItems() );
    for ( Items::iterator iter = items.begin(); iter != items.end(); ++iter )
    {
        _ui->listWidget->takeItem ( _ui->listWidget->row ( *iter ) );
        delete *iter;
    }
  
    _ui->listWidget->update();
}

void AddFileSystemWidget::onFilesSelected( const QStringList& fileNames )
{
    for ( unsigned int i = 0; i < fileNames.size(); ++i )
    {
        _ui->listWidget->addItem ( fileNames.at ( i ) );
    }
}

void AddFileSystemWidget::onFileSelected( const QString& fileName )
{
    _ui->listWidget->addItem ( fileName );
}

#endif
