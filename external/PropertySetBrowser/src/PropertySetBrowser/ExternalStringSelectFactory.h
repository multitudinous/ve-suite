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

#include <qtpropertybrowser.h>
#include <PropertySetBrowser/ExternalStringSelectManager.h>

/// @file ExternalStringSelectFactory.h
/// @namespace PropertySetBrowser
/// @class ExternalStringSelectFactory is a class factory for
/// ExternalStringSelect and derived types. There must be a separate instance
/// ExternalStringSelectFactory for each derived type. The exact type is set
/// via a call to setEditorType by passing in a pointer to an instance of the
/// ExternalStringSelect-derived editor type.

namespace PropertySetBrowser
{

class ExternalStringSelect;

class PROPERTYSETBROWSER_EXPORT ExternalStringSelectFactory : public QtAbstractEditorFactory<ExternalStringSelectManager>
{
    Q_OBJECT
public:
    ExternalStringSelectFactory(QObject *parent = 0)
        : QtAbstractEditorFactory<ExternalStringSelectManager>(parent),
          m_editorType( 0 )
            {  }
    virtual ~ExternalStringSelectFactory();

    /// Sets the editor type this factory should create. This factory will take
    /// over ownership of the pointer passed in, so don't delete it elsewhere!
    void setEditorType( ExternalStringSelect* editor );
protected:
    virtual void connectPropertyManager(ExternalStringSelectManager *manager);
    virtual QWidget *createEditor(ExternalStringSelectManager *manager, QtProperty *property,
                QWidget *parent);
    virtual void disconnectPropertyManager(ExternalStringSelectManager *manager);
private Q_SLOTS:
    void slotPropertyChanged(QtProperty *property, const QString &value);
    void slotSetValue(const QString &value);
    void slotEditorDestroyed(QObject *object);
private:
    QMap<QtProperty *, QList<ExternalStringSelect *> > theCreatedEditors;
    QMap<ExternalStringSelect *, QtProperty *> theEditorToProperty;
    ExternalStringSelect* m_editorType;
};

}

