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

#define QT_NO_KEYWORDS

#include <qttreepropertybrowser.h>
#include <PropertySetBrowser/PropertyParser.h>

/// @file GenericPropertyBrowser.h
/// @namespace PropertySetBrowser
/// @class GenericPropertyBrowser ties together a QtTreePropertyBrowser,
/// a set of editor factories, and a PropertySetParser. Use it to display
/// a tree-type property browser. If you need a different type ( such as a
/// groupbox browser ) or need to handle custom string types -- this one handles
/// file paths as an example of a custom string type -- the best approach is to
/// copy all the code from this class into a new class and then alter it to
/// create the appropriate factories, do any resizing or polish needed, etc.

namespace PropertySetBrowser
{

class PROPERTYSETBROWSER_EXPORT GenericPropertyBrowser : public QtTreePropertyBrowser
{
Q_OBJECT
public:
    explicit GenericPropertyBrowser(QWidget* parent = 0);

    void SetPropertyParser( PropertyParser* parser );

    /// Refreshes the list of properties to be displayed. This does not refresh
    /// the *values* of the property-value pairs, but only refreshes the property
    /// labels and the value types.
    /// @param autosize When true autosizes the column widths to attempt to
    ///                 display all information. When false, renders the property
    ///                 and value columns as equal widths.
    void RefreshContents( bool autosize = true );

    void ParsePropertySet( PropertySetPtr set );

    /// Refreshes only values of property-value pairs that have changed.
    void RefreshValues();

    /// Re-reads values of all property-value pairs. More expensive operation
    /// than RefreshValues();
    void RefreshAllValues();

Q_SIGNALS:

public Q_SLOTS:

private:
    PropertyParser* mParser;
    QtDoubleSpinBoxFactory* mDoubleSpinBoxFactory;
    QtSpinBoxFactory* mSpinBoxFactory;
    QtCheckBoxFactory* mCheckBoxFactory;
    QtLineEditFactory* mLineEditFactory;
    QtEnumEditorFactory* mComboBoxFactory;
    QtSliderFactory* mSliderFactory;
    ExternalStringSelectFactory* mFileEditFactory;
    //ExternalStringSelectFactory* mNodeSelectFactory;
};

} // namespace
