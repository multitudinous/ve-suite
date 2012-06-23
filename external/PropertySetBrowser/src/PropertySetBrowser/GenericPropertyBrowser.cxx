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
#define QT_NO_KEYWORDS

#include <qtpropertymanager.h>
#include <qteditorfactory.h>

#include <PropertySetBrowser/GenericPropertyBrowser.h>

#include <iostream>

namespace PropertySetBrowser
{

GenericPropertyBrowser::GenericPropertyBrowser(QWidget* parent) :
    QtTreePropertyBrowser(parent),
    mParser( 0 )
{
    // Create the default editor types we want to use
    mDoubleSpinBoxFactory = new QtDoubleSpinBoxFactory(this);
    mSpinBoxFactory = new QtSpinBoxFactory(this);
    mCheckBoxFactory = new QtCheckBoxFactory(this);
    mLineEditFactory = new QtLineEditFactory(this);
    mComboBoxFactory = new QtEnumEditorFactory(this);
    mSliderFactory = new QtSliderFactory(this);

    // Set the type for mFileEditFactory. A similar procedure can be used to
    // set up other custom ExternalStringSelect types
    mFileEditFactory = new ExternalStringSelectFactory(this);
    mFileEditFactory->setEditorType( new FileEdit );
}

void GenericPropertyBrowser::ParsePropertySet( PropertySetPtr set )
{
    if( !mParser )
    {
        SetPropertyParser( new PropertyParser );
    }

    mParser->ParsePropertySet( set );
    RefreshAllValues();
    RefreshContents();
}

void GenericPropertyBrowser::SetPropertyParser( PropertyParser* parser )
{
    mParser = parser;

    // Associate editor types with property types
    this->setFactoryForManager( parser->GetBoolManager(), mCheckBoxFactory );
    this->setFactoryForManager( parser->GetIntManager(), mSpinBoxFactory );
    this->setFactoryForManager( parser->GetDoubleManager(), mDoubleSpinBoxFactory );
    this->setFactoryForManager( parser->GetStringManager(), mLineEditFactory );
    this->setFactoryForManager( parser->GetEnumManager(), mComboBoxFactory );

    ExternalStringSelectManager* filePathManager = new ExternalStringSelectManager;
    parser->AddCustomExternalStringManager( filePathManager, "isFilePath" );
    this->setFactoryForManager( filePathManager, mFileEditFactory );
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
    PropertyParser::ItemVector* items = mParser->GetItems();
    PropertyParser::ItemVector::iterator iterator;
    PropertyParser::ItemVector::iterator end = items->end();
    for( iterator = items->begin(); iterator != end; iterator++)
    {
        this->addProperty( (*iterator) );
    }

    mParser->RefreshAll( );

    // Auto-sizing of columns should be complete; allow user to control widths
    this->setResizeMode(QtTreePropertyBrowser::Interactive);
}

void GenericPropertyBrowser::RefreshValues()
{
    if( mParser )
    {
        mParser->Refresh();
    }
}

void GenericPropertyBrowser::RefreshAllValues()
{
    if( mParser )
    {
        mParser->RefreshAll();
    }
}

}
