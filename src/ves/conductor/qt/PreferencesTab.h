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
#include <ves/xplorer/data/PropertySetPtr.h>

#include <QtGui/QWidget>
#include <QtCore/QAbstractItemModel>
#include <QtCore/QModelIndex>


namespace Ui {
    class PreferencesTab;
}


/*!\file PreferencesTab.h
 * Preferences Tab
 * class ves::conductor::PreferencesTab
 * This class manages the preferences for Xplorer Qt window.
 * \namespace ves::conductor
 * UI Namespace
 */
namespace ves
{
namespace conductor
{
class PropertyBrowser;

class PreferencesTab : public QWidget {
    Q_OBJECT
public:
    PreferencesTab(QWidget *parent = 0);
    ~PreferencesTab();

protected:
    void changeEvent(QEvent *e);

protected Q_SLOTS:

private:
    Ui::PreferencesTab *ui;
    
    ves::xplorer::data::PropertySetPtr m_propertySet;
    
    PropertyBrowser* m_featureBrowser;
};

} // namespace conductor
} // namespace ves
