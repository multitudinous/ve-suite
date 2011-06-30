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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/conductor/qt/RecentFiles.h>
#include <ves/conductor/qt/ui_RecentFiles.h>

#include <QtCore/QSettings>
#include <QtGui/QPushButton>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
RecentFiles::RecentFiles(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::RecentFiles)
{
    ui->setupUi(this);

    QPushButton* clearButton = new QPushButton( tr("Clear List") );
    connect( clearButton, SIGNAL(clicked()), this, SLOT(Clear()) );
    ui->buttonBox->addButton( clearButton, QDialogButtonBox::ResetRole );

    QSettings settings( QSettings::IniFormat, QSettings::UserScope,
                            "VE Suite", "VE Xplorer" );
    QStringList files = settings.value("recentFileList").toStringList();
    ui->m_recentFilesList->addItems( files );
}
////////////////////////////////////////////////////////////////////////////////
RecentFiles::~RecentFiles()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void RecentFiles::changeEvent(QEvent *e)
{
    QDialog::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
const QString& RecentFiles::GetSelectedFile()
{
    selectedFile = "";
    QListWidgetItem* selected = ui->m_recentFilesList->currentItem();
    if( selected )
    {
        selectedFile = selected->data(0).toString();
    }

    return selectedFile;
}
////////////////////////////////////////////////////////////////////////////////
void RecentFiles::Clear()
{
    QSettings settings( QSettings::IniFormat, QSettings::UserScope,
                            "VE Suite", "VE Xplorer" );
    QStringList files = settings.value("recentFileList").toStringList();
    files.clear();
    settings.setValue( "recentFileList", files );

    ui->m_recentFilesList->clear();
}
////////////////////////////////////////////////////////////////////////////////
}
}
