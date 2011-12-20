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
#define QT_NO_KEYWORDS

#include <qtpropertymanager.h>
#include <qteditorfactory.h>

#include <ves/conductor/qt/propertyBrowser/GenericPropertyBrowser.h>

#include <iostream>

using namespace ves::conductor;

GenericPropertyBrowser::GenericPropertyBrowser(QWidget* parent) :
    QtTreePropertyBrowser(parent)
{
    // Create the default editor types we want to use
    mDoubleSpinBoxFactory = new QtDoubleSpinBoxFactory(this);
    mSpinBoxFactory = new QtSpinBoxFactory(this);
    mCheckBoxFactory = new QtCheckBoxFactory(this);
    mLineEditFactory = new QtLineEditFactory(this);
    mComboBoxFactory = new QtEnumEditorFactory(this);
    mSliderFactory = new QtSliderFactory(this);
    mFileEditFactory = new FileEditFactory(this);
    mNodeSelectFactory = new NodeSelectFactory(this);
}

void GenericPropertyBrowser::setPropertyBrowser( PropertyBrowser* browser )
{
    mBrowser = browser;

    // Associate editor types with property types
    this->setFactoryForManager( browser->GetBoolManager(), mCheckBoxFactory );
    this->setFactoryForManager( browser->GetIntManager(), mSpinBoxFactory );
    this->setFactoryForManager( browser->GetDoubleManager(), mDoubleSpinBoxFactory );
    this->setFactoryForManager( browser->GetStringManager(), mLineEditFactory );
    this->setFactoryForManager( browser->GetEnumManager(), mComboBoxFactory );
    this->setFactoryForManager( browser->GetFilePathManager(), mFileEditFactory );
    this->setFactoryForManager( browser->GetNodeSelectManager(), mNodeSelectFactory );
}

void GenericPropertyBrowser::RefreshContents( bool autosize )
{
    // Clear out anything currently in the browser
    this->clear();

    // Tell the browser to auto-resize column widths
    if( autosize )
    {
        this->setResizeMode(QtTreePropertyBrowser::ResizeToContents);
    }
    else
    {
        this->setResizeMode(QtTreePropertyBrowser::Interactive);
        this->setSplitterPosition( 175 );
    }

    // Get all the QItems from underlying propertybrowser and add to this view
    PropertyBrowser::ItemVector* items = mBrowser->GetItems();
    PropertyBrowser::ItemVector::iterator iterator;
    PropertyBrowser::ItemVector::iterator end = items->end();
    for( iterator = items->begin(); iterator != end; iterator++)
    {
        this->addProperty( (*iterator) );
    }

    mBrowser->RefreshAll( );

    // Auto-sizing of columns should be complete; allow user to control widths
    this->setResizeMode(QtTreePropertyBrowser::Interactive);
}
