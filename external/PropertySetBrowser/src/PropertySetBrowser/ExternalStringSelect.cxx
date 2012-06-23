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
#include <PropertySetBrowser/ExternalStringSelect.h>
#include <QtGui/QHBoxLayout>
#include <QtGui/QToolButton>
#include <QtGui/QFileDialog>
#include <QtGui/QFocusEvent>
#include <iostream>

Q_DECLARE_METATYPE(std::string)

namespace PropertySetBrowser
{

ExternalStringSelect::ExternalStringSelect(QWidget *parent)
    : QWidget(parent)
{
    qRegisterMetaType<std::string>();
    QHBoxLayout *layout = new QHBoxLayout(this);
    layout->setMargin(0);
    layout->setSpacing(0);
    theLineEdit = new QLineEdit(this);
    theLineEdit->setSizePolicy(QSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred));
    QToolButton *button = new QToolButton(this);
    button->setSizePolicy(QSizePolicy(QSizePolicy::Fixed, QSizePolicy::Preferred));
    button->setText(QLatin1String("Select..."));
    layout->addWidget(theLineEdit);
    layout->addWidget(button);
    setFocusProxy(theLineEdit);
    setFocusPolicy(Qt::StrongFocus);
    setAttribute(Qt::WA_InputMethodEnabled);
    connect(theLineEdit, SIGNAL(textEdited(const QString &)),
                this, SIGNAL(stringChanged(const QString &)));
    connect(button, SIGNAL(clicked()),
                this, SLOT(buttonClicked()));
    connect( this, SIGNAL(ExternalStringSelectedQSignal(std::string)),
             this, SLOT(onExternalStringSelectedQueued(std::string)),
             Qt::QueuedConnection );
}

ExternalStringSelect* ExternalStringSelect::createNew( QWidget* parent )
{
    return new ExternalStringSelect( parent );
}

void ExternalStringSelect::buttonClicked()
{
    // Do nothing; derived classes should override to produce desired behavior,
    // such as opening a file dialog.
}

void ExternalStringSelect::onExternalStringSelected( const std::string& str )
{
    emit ExternalStringSelectedQSignal( str );
}

void ExternalStringSelect::onExternalStringSelectedQueued( const std::string str )
{
    QString text = text.fromStdString( str );
    theLineEdit->setText( text );
    emit stringChanged(text);
}

void ExternalStringSelect::focusInEvent(QFocusEvent *e)
{
    theLineEdit->event(e);
    if (e->reason() == Qt::TabFocusReason || e->reason() == Qt::BacktabFocusReason) {
        theLineEdit->selectAll();
    }
    QWidget::focusInEvent(e);
}

void ExternalStringSelect::focusOutEvent(QFocusEvent *e)
{
    theLineEdit->event(e);
    QWidget::focusOutEvent(e);
}

void ExternalStringSelect::keyPressEvent(QKeyEvent *e)
{
    theLineEdit->event(e);
}

void ExternalStringSelect::keyReleaseEvent(QKeyEvent *e)
{
    theLineEdit->event(e);
}

}
