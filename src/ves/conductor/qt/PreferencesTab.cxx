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
#include <ves/conductor/qt/PreferencesTab.h>

#include <ves/conductor/qt/ui_PreferencesTab.h>

#include <ves/xplorer/data/PreferencesPropertySet.h>

#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>

#include <iostream>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
PreferencesTab::PreferencesTab(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::PreferencesTab)
{
    ui->setupUi(this);
    
    m_featureBrowser = new PropertyBrowser( this );

    m_propertySet = ves::xplorer::data::PropertySetPtr( new ves::xplorer::data::PreferencesPropertySet() );
    m_propertySet->WriteToDatabase();
    
    // Let the PropertyBrowser container pars the initial property set
    m_featureBrowser->ParsePropertySet( m_propertySet );
    
    // ui.preferencesPropertyBrowser is an instance of GenericPropertyBrowser, 
    // which knows how
    // to take the Qt-ized data from a PropertyBrowser such as
    // m_featureBrowser and display it in the GUI.
    ui->preferencesPropertyBrowser->setPropertyBrowser( m_featureBrowser );
    ui->preferencesPropertyBrowser->RefreshContents();
    ui->preferencesPropertyBrowser->show();
    
    // No need to load before parsing, since values in browser are not
    // set during parsing. They're only set by signals from the property
    // set when things changed, which loading will do. But this doesn't
    // work until after parsing is complete.
    m_propertySet->LoadFromDatabase();
    m_featureBrowser->RefreshAll();
}
////////////////////////////////////////////////////////////////////////////////
PreferencesTab::~PreferencesTab()
{
    delete ui;
    delete m_featureBrowser;
}
////////////////////////////////////////////////////////////////////////////////
void PreferencesTab::changeEvent(QEvent *e)
{
    QWidget::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}

} // namespace conductor
} // namespace ves
