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
#include <QtGui/QFont>
#include <QtGui/QColor>
#include <QtGui/QBrush>

#include <iostream>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
RecentFiles::RecentFiles(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::RecentFiles),
    m_lastChanged( 0 )
{
    ui->setupUi(this);

    QPushButton* clearButton = new QPushButton( tr("Clear List") );
    connect( clearButton, SIGNAL(clicked()), this, SLOT(Clear()) );
    ui->buttonBox->addButton( clearButton, QDialogButtonBox::ResetRole );

    QPushButton* openButton = new QPushButton( tr("Open Project...") );
    connect( openButton, SIGNAL(clicked()), this, SIGNAL(openProject()) );
    ui->buttonBox->addButton( openButton, QDialogButtonBox::ActionRole );

    QPushButton* newButton = new QPushButton( tr("New Project...") );
    connect( newButton, SIGNAL(clicked()), this, SIGNAL(newProject()) );
    ui->buttonBox->addButton( newButton, QDialogButtonBox::ActionRole );

    QSettings settings( QSettings::IniFormat, QSettings::UserScope,
                            "VE Suite", "VE Xplorer" );

    QStringList files = settings.value("recentProjectList").toStringList();
    QListWidget* recent = ui->m_recentFilesList;
    recent->addItem( tr("Recent Projects") );
    QFont font = recent->item( 0 )->font();
    font.setPointSize( font.pointSize() + 2 );
    font.setWeight( QFont::Bold );
    recent->item(0)->setFont( font );
    recent->addItems( files );

    files = settings.value("recentCADList").toStringList();
    recent->addItem( tr("") );
    recent->addItem( tr("Recent CAD") );
    recent->item( (recent->count() - 1) )->setFont( font );
    recent->addItems( files );

    files = settings.value("recentDataList").toStringList();
    recent->addItem( tr("") );
    recent->addItem( tr("Recent Datasets") );
    recent->item( (recent->count() - 1) )->setFont( font );
    recent->addItems( files );

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
    m_selectedFile.clear();
    QListWidgetItem* selected = ui->m_recentFilesList->currentItem();
    if( selected )
    {
        m_selectedFile = selected->data(0).toString();
        if( m_selectedFile == "Recent Projects" || m_selectedFile == "Recent CAD"
            || m_selectedFile == "Recent Datasets" )
        {
            m_selectedFile.clear();
        }
    }

    return m_selectedFile;
}
////////////////////////////////////////////////////////////////////////////////
void RecentFiles::Clear()
{
    QSettings settings( QSettings::IniFormat, QSettings::UserScope,
                            "VE Suite", "VE Xplorer" );
    QStringList files = settings.value("recentFileList").toStringList();
    files.clear();
    settings.setValue( "recentProjectList", files );
    settings.setValue( "recentCADList", files );
    settings.setValue( "recentDataList", files );

    ui->m_recentFilesList->clear();
}
////////////////////////////////////////////////////////////////////////////////
void RecentFiles::on_m_recentFilesList_itemEntered( QListWidgetItem* item )
{
    // Restore the color of the most recently changed item
    if( m_lastChanged )
    {
        QFont font = m_lastChanged->font();
        font.setUnderline( false );
        m_lastChanged->setFont( font );
        m_lastChanged->setForeground( QBrush(QColor(0,0,0)) );
        m_lastChanged = 0;
    }

    if( item->text() != "Recent Projects" && item->text() != "Recent CAD" &&
        item->text() != "Recent Datasets" )
    {
        QFont font = item->font();
        font.setUnderline( true );
        item->setFont( font );
        item->setForeground( QBrush(QColor(0,0,255)) );
        m_lastChanged = item;
    }
}

}
}
