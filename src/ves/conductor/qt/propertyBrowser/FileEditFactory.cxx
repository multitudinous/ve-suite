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

#include <ves/conductor/qt/propertyBrowser/FileEditFactory.h>
#include <ves/conductor/qt/propertyBrowser/FileEdit.h>

namespace ves
{
namespace conductor
{

FileEditFactory::~FileEditFactory()
{
    QList<FileEdit*> editors = theEditorToProperty.keys();
    QListIterator<FileEdit*> it( editors );
    while( it.hasNext() )
    {
        delete it.next();
    }
}

void FileEditFactory::connectPropertyManager( FilePathManager* manager )
{
    connect( manager, SIGNAL( valueChanged( QtProperty*, const QString& ) ),
             this, SLOT( slotPropertyChanged( QtProperty*, const QString& ) ) );
    connect( manager, SIGNAL( filterChanged( QtProperty*, const QString& ) ),
             this, SLOT( slotFilterChanged( QtProperty*, const QString& ) ) );
}

QWidget* FileEditFactory::createEditor( FilePathManager* manager,
                                        QtProperty* property, QWidget* parent )
{
    FileEdit* editor = new FileEdit( parent );
    editor->setFilePath( manager->value( property ) );
    editor->setFilter( manager->filter( property ) );
    theCreatedEditors[property].append( editor );
    theEditorToProperty[editor] = property;

    connect( editor, SIGNAL( filePathChanged( const QString& ) ),
             this, SLOT( slotSetValue( const QString& ) ) );
    connect( editor, SIGNAL( destroyed( QObject* ) ),
             this, SLOT( slotEditorDestroyed( QObject* ) ) );
    return editor;
}

void FileEditFactory::disconnectPropertyManager( FilePathManager* manager )
{
    disconnect( manager, SIGNAL( valueChanged( QtProperty*, const QString& ) ),
                this, SLOT( slotPropertyChanged( QtProperty*, const QString& ) ) );
    disconnect( manager, SIGNAL( filterChanged( QtProperty*, const QString& ) ),
                this, SLOT( slotFilterChanged( QtProperty*, const QString& ) ) );
}

void FileEditFactory::slotPropertyChanged( QtProperty* property,
        const QString& value )
{
    if( !theCreatedEditors.contains( property ) )
    {
        return;
    }

    QList<FileEdit*> editors = theCreatedEditors[property];
    QListIterator<FileEdit*> itEditor( editors );
    while( itEditor.hasNext() )
    {
        itEditor.next()->setFilePath( value );
    }
}

void FileEditFactory::slotFilterChanged( QtProperty* property,
        const QString& filter )
{
    if( !theCreatedEditors.contains( property ) )
    {
        return;
    }

    QList<FileEdit*> editors = theCreatedEditors[property];
    QListIterator<FileEdit*> itEditor( editors );
    while( itEditor.hasNext() )
    {
        itEditor.next()->setFilter( filter );
    }
}

void FileEditFactory::slotSetValue( const QString& value )
{
    QObject* object = sender();
    QMap<FileEdit*, QtProperty*>::ConstIterator itEditor =
        theEditorToProperty.constBegin();
    while( itEditor != theEditorToProperty.constEnd() )
    {
        if( itEditor.key() == object )
        {
            QtProperty* property = itEditor.value();
            FilePathManager* manager = propertyManager( property );
            if( !manager )
            {
                return;
            }
            manager->setValue( property, value );
            return;
        }
        itEditor++;
    }
}

void FileEditFactory::slotEditorDestroyed( QObject* object )
{
    QMap<FileEdit*, QtProperty*>::ConstIterator itEditor =
        theEditorToProperty.constBegin();
    while( itEditor != theEditorToProperty.constEnd() )
    {
        if( itEditor.key() == object )
        {
            FileEdit* editor = itEditor.key();
            QtProperty* property = itEditor.value();
            theEditorToProperty.remove( editor );
            theCreatedEditors[property].removeAll( editor );
            if( theCreatedEditors[property].isEmpty() )
            {
                theCreatedEditors.remove( property );
            }
            return;
        }
        itEditor++;
    }
}

}
}
