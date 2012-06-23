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
#pragma once

/** This class is a slightly altered version of the FileEdit class shown in
 Qt Quarterly at
http://doc.qt.nokia.com/qq/qq18-propertybrowser.html#extendingtheframework
It is being used in accordance with the terms of LGPL **/


#include <QtGui/QLineEdit>
#include <QtGui/QFileDialog>
#include <PropertySetBrowser/Exports.h>

/// @file ExternalStringSelect.h
/// @namespace PropertySetBrowser
/// @class ExternalStringSelect is a base implementation of an external string
/// selector widget consisting of a textedit field and an associated button.
/// In the base implementation, the button does nothing; the buttonClicked
/// method should be overridden in derived classes to provide the desired
/// behavior for externally selecting a string. Examples of this behavior are
/// launching a file chooser dialog or launching a color chooser dialog. The
/// execution path taken when the user clicks the button should eventually call
/// onExternalStringSelected() with the contents of the chosen string. Derived
/// classes must override buttonClicked and createNew.

namespace PropertySetBrowser
{

class PROPERTYSETBROWSER_EXPORT ExternalStringSelect : public QWidget
{
    Q_OBJECT
public:
    ExternalStringSelect(QWidget *parent = 0);
    void setString(const QString &str) { if (theLineEdit->text() != str) theLineEdit->setText(str); }
    QString string() const { return theLineEdit->text(); }

    /// Returns a pointer to a new one of these. Used by ExternalStringSelectFactory
    /// to create new versions of this object. This allows the developer to derive
    /// from this class, and pass a pointer to the derived class to
    /// ExternalStringSelectFactory::setEditorType. The factory can then create
    /// instances of the derived class as needed.
    virtual ExternalStringSelect* createNew( QWidget* parent );
Q_SIGNALS:
    void stringChanged(const QString &str);
    void ExternalStringSelectedQSignal( const std::string str );

protected:
    void focusInEvent(QFocusEvent *e);
    void focusOutEvent(QFocusEvent *e);
    void keyPressEvent(QKeyEvent *e);
    void keyReleaseEvent(QKeyEvent *e);

public Q_SLOTS:
    virtual void buttonClicked();
    virtual void onExternalStringSelected( const std::string& str );
    virtual void onExternalStringSelectedQueued( const std::string str );

private:
    QLineEdit *theLineEdit;
};

}

