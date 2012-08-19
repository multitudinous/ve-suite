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
#define VES_DEBUG
#include <ves/conductor/qt/propertyBrowser/NodeSelect.h>
#include <QtGui/QHBoxLayout>
#include <QtGui/QToolButton>
#include <QtGui/QFileDialog>
#include <QtGui/QFocusEvent>

#include <ves/conductor/qt/UITabs.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

namespace ves
{
namespace conductor
{

NodeSelect::NodeSelect( QWidget* parent )
    : QWidget( parent ),
      m_logger( Poco::Logger::get( "conductor.NodeSelect" ) ),
      m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    LOG_DEBUG( "Ctor" );
    QHBoxLayout* layout = new QHBoxLayout( this );
    layout->setMargin( 0 );
    layout->setSpacing( 0 );
    theLineEdit = new QLineEdit( this );
    theLineEdit->setSizePolicy( QSizePolicy( QSizePolicy::Expanding, QSizePolicy::Preferred ) );
    QToolButton* button = new QToolButton( this );
    button->setSizePolicy( QSizePolicy( QSizePolicy::Fixed, QSizePolicy::Preferred ) );
    button->setText( QLatin1String( "Select node..." ) );
    layout->addWidget( theLineEdit );
    layout->addWidget( button );
    setFocusProxy( theLineEdit );
    setFocusPolicy( Qt::StrongFocus );
    setAttribute( Qt::WA_InputMethodEnabled );
    connect( theLineEdit, SIGNAL( textEdited( const QString& ) ),
             this, SIGNAL( filePathChanged( const QString& ) ) );
    connect( button, SIGNAL( clicked() ),
             this, SLOT( buttonClicked() ) );
    connect( this, SIGNAL( nodeSelectedQSignal( std::string ) ),
             this, SLOT( onNodeSelectedQueued( std::string ) ),
             Qt::QueuedConnection );
}

void NodeSelect::buttonClicked()
{
    LOG_DEBUG( "buttonClicked" );
    ves::conductor::UITabs::instance()->ActivateTab( "Layers" );
    CONNECTSIGNALS_1( "%CADNodeSelected",
                      void( std::string const& ),
                      &NodeSelect::onNodeSelected,
                      m_connections, any_SignalType, normal_Priority );
}

void NodeSelect::onNodeSelected( const std::string& nodePath )
{
    LOG_DEBUG( "onNodeSelected" );
    emit nodeSelectedQSignal( nodePath );
}

void NodeSelect::onNodeSelectedQueued( const std::string nodePath )
{
    LOG_DEBUG( "onNodeSelectedQueued, nodePath = " << nodePath );
    m_connections.DropConnections();
    ves::conductor::UITabs::instance()->ActivateTab( "Constraints" );

    QString text = text.fromStdString( nodePath );
    theLineEdit->setText( text );
    emit filePathChanged( text );
}

void NodeSelect::focusInEvent( QFocusEvent* e )
{
    theLineEdit->event( e );
    if( e->reason() == Qt::TabFocusReason || e->reason() == Qt::BacktabFocusReason )
    {
        theLineEdit->selectAll();
    }
    QWidget::focusInEvent( e );
}

void NodeSelect::focusOutEvent( QFocusEvent* e )
{
    theLineEdit->event( e );
    QWidget::focusOutEvent( e );
}

void NodeSelect::keyPressEvent( QKeyEvent* e )
{
    theLineEdit->event( e );
}

void NodeSelect::keyReleaseEvent( QKeyEvent* e )
{
    theLineEdit->event( e );
}

}
}
