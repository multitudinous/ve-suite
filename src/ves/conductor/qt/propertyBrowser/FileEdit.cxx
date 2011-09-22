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

#include <ves/conductor/qt/propertyBrowser/FileEdit.h>
#include <QtGui/QHBoxLayout>
#include <QtGui/QToolButton>
#include <QtGui/QFileDialog>
#include <QtGui/QFocusEvent>

#include <ves/conductor/qt/UITabs.h>

namespace ves
{
namespace conductor
{

FileEdit::FileEdit(QWidget *parent)
    : QWidget(parent),
    m_fileDialog( 0 )
{
    QHBoxLayout *layout = new QHBoxLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);
    theLineEdit = new QLineEdit(this);
    theLineEdit->setSizePolicy(QSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred));
    QToolButton *button = new QToolButton(this);
    button->setSizePolicy(QSizePolicy(QSizePolicy::Fixed, QSizePolicy::Preferred));
    button->setText(QLatin1String("..."));
    layout->addWidget(theLineEdit);
    layout->addWidget(button);
    setFocusProxy(theLineEdit);
    setFocusPolicy(Qt::StrongFocus);
    setAttribute(Qt::WA_InputMethodEnabled);
    connect(theLineEdit, SIGNAL(textEdited(const QString &)),
                this, SIGNAL(filePathChanged(const QString &)));
    connect(button, SIGNAL(clicked()),
                this, SLOT(buttonClicked()));
}

void FileEdit::buttonClicked()
{
    ves::conductor::UITabs* tabs = ves::conductor::UITabs::instance();

    if( m_fileDialog )
    {
        tabs->ActivateTab( m_fileDialog );
        return;
    }

    m_fileDialog = new QFileDialog( 0 );
    m_fileDialog->setOptions( QFileDialog::DontUseNativeDialog );
    m_fileDialog->setAttribute( Qt::WA_DeleteOnClose );
    m_fileDialog->setFileMode( QFileDialog::ExistingFile );

    //m_fileDialog->setNameFilters( theFilter );

    connect( m_fileDialog, SIGNAL(fileSelected(const QString &)),
                      this, SLOT(onFileSelected(const QString&)) );
    connect( m_fileDialog, SIGNAL(rejected()), this,
                      SLOT( onFileCancelled() ) );

    tabs->ActivateTab( tabs->AddTab( m_fileDialog, "Select File" ) );
}

void FileEdit::onFileSelected( const QString& filePath )
{

    QDir path = QDir::current();
    QString relativePath = path.relativeFilePath( filePath );
    theLineEdit->setText( relativePath );

    ves::conductor::UITabs::instance()->RemoveTab( m_fileDialog );

    if ( m_fileDialog != 0 )
    {
        m_fileDialog->close();
        m_fileDialog = 0;
    }

    emit filePathChanged( relativePath );
}

void FileEdit::onFileCancelled()
{
    ves::conductor::UITabs::instance()->RemoveTab( m_fileDialog );

    if ( m_fileDialog != 0 )
    {
        m_fileDialog->close();
        m_fileDialog = 0;
    }
}

void FileEdit::focusInEvent(QFocusEvent *e)
{
    theLineEdit->event(e);
    if (e->reason() == Qt::TabFocusReason || e->reason() == Qt::BacktabFocusReason) {
        theLineEdit->selectAll();
    }
    QWidget::focusInEvent(e);
}

void FileEdit::focusOutEvent(QFocusEvent *e)
{
    theLineEdit->event(e);
    QWidget::focusOutEvent(e);
}

void FileEdit::keyPressEvent(QKeyEvent *e)
{
    theLineEdit->event(e);
}

void FileEdit::keyReleaseEvent(QKeyEvent *e)
{
    theLineEdit->event(e);
}

}
}
