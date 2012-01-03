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
#include <ves/conductor/qt/propertyBrowser/NodeSelectManager.h>

namespace ves
{
namespace conductor
{

class NodeSelect;

class NodeSelectFactory : public QtAbstractEditorFactory<NodeSelectManager>
{
    Q_OBJECT
public:
    NodeSelectFactory(QObject *parent = 0)
        : QtAbstractEditorFactory<NodeSelectManager>(parent)
            { }
    virtual ~NodeSelectFactory();
protected:
    virtual void connectPropertyManager(NodeSelectManager *manager);
    virtual QWidget *createEditor(NodeSelectManager *manager, QtProperty *property,
                QWidget *parent);
    virtual void disconnectPropertyManager(NodeSelectManager *manager);
private Q_SLOTS:
    void slotPropertyChanged(QtProperty *property, const QString &value);
    void slotSetValue(const QString &value);
    void slotEditorDestroyed(QObject *object);
private:
    QMap<QtProperty *, QList<NodeSelect *> > theCreatedEditors;
    QMap<NodeSelect *, QtProperty *> theEditorToProperty;
};

}
}
