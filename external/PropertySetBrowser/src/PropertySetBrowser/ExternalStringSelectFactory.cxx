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

#include <PropertySetBrowser/ExternalStringSelectFactory.h>
#include <PropertySetBrowser/ExternalStringSelect.h>

namespace PropertySetBrowser
{

ExternalStringSelectFactory::~ExternalStringSelectFactory()
{
    QList<ExternalStringSelect *> editors = theEditorToProperty.keys();
    QListIterator<ExternalStringSelect *> it(editors);
    while (it.hasNext())
        delete it.next();
    delete m_editorType;
}

void ExternalStringSelectFactory::connectPropertyManager(ExternalStringSelectManager *manager)
{
    connect(manager, SIGNAL(valueChanged(QtProperty *, const QString &)),
                this, SLOT(slotPropertyChanged(QtProperty *, const QString &)));
}

void ExternalStringSelectFactory::setEditorType( ExternalStringSelect* editor )
{
    m_editorType = editor;
}

QWidget *ExternalStringSelectFactory::createEditor(ExternalStringSelectManager *manager,
        QtProperty *property, QWidget *parent)
{
    //ExternalStringSelect *editor = new ExternalStringSelect(parent);
    // Create new instance from the type set in setEditorType.
    ExternalStringSelect* editor = m_editorType->createNew( parent );
    editor->setString(manager->value(property));
    theCreatedEditors[property].append(editor);
    theEditorToProperty[editor] = property;

    connect(editor, SIGNAL(stringChanged(const QString &)),
                this, SLOT(slotSetValue(const QString &)));
    connect(editor, SIGNAL(destroyed(QObject *)),
                this, SLOT(slotEditorDestroyed(QObject *)));
    return editor;
}

void ExternalStringSelectFactory::disconnectPropertyManager(ExternalStringSelectManager *manager)
{
    disconnect(manager, SIGNAL(valueChanged(QtProperty *, const QString &)),
                this, SLOT(slotPropertyChanged(QtProperty *, const QString &)));
}

void ExternalStringSelectFactory::slotPropertyChanged(QtProperty *property,
                const QString &value)
{
    if (!theCreatedEditors.contains(property))
        return;

    QList<ExternalStringSelect *> editors = theCreatedEditors[property];
    QListIterator<ExternalStringSelect *> itEditor(editors);
    while (itEditor.hasNext())
        itEditor.next()->setString(value);
}


void ExternalStringSelectFactory::slotSetValue(const QString &value)
{
    QObject *object = sender();
    QMap<ExternalStringSelect *, QtProperty *>::ConstIterator itEditor =
                theEditorToProperty.constBegin();
    while (itEditor != theEditorToProperty.constEnd()) {
        if (itEditor.key() == object) {
            QtProperty *property = itEditor.value();
            ExternalStringSelectManager *manager = propertyManager(property);
            if (!manager)
                return;
            manager->setValue(property, value);
            return;
        }
        itEditor++;
    }
}

void ExternalStringSelectFactory::slotEditorDestroyed(QObject *object)
{
    QMap<ExternalStringSelect *, QtProperty *>::ConstIterator itEditor =
                theEditorToProperty.constBegin();
    while (itEditor != theEditorToProperty.constEnd()) {
        if (itEditor.key() == object) {
            ExternalStringSelect *editor = itEditor.key();
            QtProperty *property = itEditor.value();
            theEditorToProperty.remove(editor);
            theCreatedEditors[property].removeAll(editor);
            if (theCreatedEditors[property].isEmpty())
                theCreatedEditors.remove(property);
            return;
        }
        itEditor++;
    }
}

}

