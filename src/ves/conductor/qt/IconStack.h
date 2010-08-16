/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef ICONSTACK_H
#define ICONSTACK_H

#include <QtGui/QToolBar>

class IconStack : public QToolBar
{
Q_OBJECT
public:
    explicit IconStack( QWidget* positionParent, QWidget* parent = 0 );

    /// Sets the position of the toolbar based on the positionParent,
    /// auto-adjusts the size, and shows the toolbar. If you want simple show/hide
    /// functionality, use QToolBar::show() <-- Notice lowercase.
    void Show();

    /// Controls whether the toolbar disappears when one of its buttons is pressed
    /// or whether it stays visible. Default is false, meaning the toolbar
    /// disappears.
    void SetPersistence( bool persistence );

    /// "Overrides" of the similarly-named methods of QToolBar. Please use these
    /// instead of QToolBar::addAction( ... ) methods.
    void AddAction ( QAction* action );
    QAction* AddAction ( const QString& text );
    QAction* AddAction ( const QIcon& icon, const QString& text );
    QAction* AddAction ( const QString& text, const QObject* receiver, const char* member );
    QAction* AddAction ( const QIcon& icon, const QString& text, const QObject* receiver, const char* member );


Q_SIGNALS:

public Q_SLOTS:

private:
    QWidget* mPositionParent;
    bool mPersistent;

    QAction* _connectAction( QAction* action );

};

#endif // ICONSTACK_H
