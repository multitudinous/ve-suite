/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/conductor/qt/IconStack.h>

#include <QtGui/QAction>

#include <iostream>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
IconStack::IconStack( QWidget* positionParent, QWidget* parent )
    :
    QToolBar(parent),
    mPositionParent(positionParent),
    mPersistent(false)
{
    setOrientation( Qt::Vertical );
    setMovable( false );
    setFloatable( false );
    setIconSize( QSize( 32, 32 ) );
    setAutoFillBackground( true );
    hide();
}
////////////////////////////////////////////////////////////////////////////////
void IconStack::Show()
{
    if( mPositionParent )
    {
         QPoint pos( 0, mPositionParent->height() );
         QPoint gPos = mPositionParent->mapToGlobal( pos );  
         QPoint mPos = parentWidget()->mapFromGlobal( gPos );
         move( mPos );
    }
    show();
    raise();
    adjustSize();
}
////////////////////////////////////////////////////////////////////////////////
void IconStack::SetPersistence( bool persistence )
{
    mPersistent = persistence;
}
////////////////////////////////////////////////////////////////////////////////
void IconStack::AddAction ( QAction* action )
{
    addAction( action );
    _connectAction( action );
}
////////////////////////////////////////////////////////////////////////////////
QAction* IconStack::AddAction ( const QString& text )
{
    return _connectAction( addAction( text ) );
}
////////////////////////////////////////////////////////////////////////////////
QAction* IconStack::AddAction ( const QIcon& icon, const QString& text )
{
    return _connectAction( addAction( icon, text ) );
}
////////////////////////////////////////////////////////////////////////////////
QAction* IconStack::AddAction ( const QString& text, const QObject* receiver, const char* member )
{
    return _connectAction( addAction( text, receiver, member ) );
}
////////////////////////////////////////////////////////////////////////////////
QAction* IconStack::AddAction ( const QIcon& icon, const QString& text, const QObject* receiver, const char* member )
{
   return  _connectAction( addAction( icon, text, receiver, member ) );
}
////////////////////////////////////////////////////////////////////////////////
QAction* IconStack::_connectAction( QAction* action )
{
    if( !mPersistent )
    {
        QObject::connect( action, SIGNAL(triggered(bool)), SLOT(hide()) );
    }
    return action;
}
////////////////////////////////////////////////////////////////////////////////
}
}
