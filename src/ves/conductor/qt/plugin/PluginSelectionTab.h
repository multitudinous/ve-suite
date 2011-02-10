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

#pragma once

#define QT_NO_KEYWORDS

#include <QtGui/QWidget>
#include <map>

namespace Ui {
    class PluginSelectionTab;
}

class QListWidgetItem;

namespace ves
{
namespace conductor
{
class MainWindow;
class UIPluginInterface;


class PluginSelectionTab : public QWidget
{
    Q_OBJECT

public:
    explicit PluginSelectionTab( MainWindow* mainWindow, QWidget *parent = 0 );
    ~PluginSelectionTab();

protected:
    void changeEvent(QEvent *e);

private:
    Ui::PluginSelectionTab *ui;
    MainWindow* m_mainWindow;

    std::map< QListWidgetItem*, UIPluginInterface* > m_itemInterfaceMap;

    void InstantiatePlugin( QListWidgetItem* item );

    std::map< QListWidgetItem*, QWidget* > m_itemWidgetMap;

protected Q_SLOTS:
    // Auto-connected slot
    void on_m_addPluginButton_clicked();
    void on_m_removePluginButton_clicked();
    void on_m_instantiatedPlugins_itemDoubleClicked( QListWidgetItem* item );
    void on_m_availablePlugins_itemDoubleClicked( QListWidgetItem* item );
    void on_m_instantiatedPlugins_itemChanged ( QListWidgetItem * item );
};

}
}
