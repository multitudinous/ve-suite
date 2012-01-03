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

#include <ves/xplorer/eventmanager/ScopedConnectionList.h>

#include <QtGui/QWidget>
#include <map>

namespace Ui {
    class PluginSelectionTab;
}

class QListWidgetItem;

namespace ves
{
namespace xplorer
{
namespace plugin
{
class PluginBase;
}
}
namespace conductor
{
class MainWindow;
class UIPluginInterface;


class PluginSelectionTab : public QWidget
{
    Q_OBJECT

public:
    explicit PluginSelectionTab( MainWindow* mainWindow, QWidget *parent = 0 );
    ///Destructor
    virtual ~PluginSelectionTab();
    ///Clear the plugins
    void ClearActivePlugins();
    ///Search for plugins in the specified directory
    void DiscoverPlugins( std::string const& dir );
    ///Search for plugins from the Change working directory signal
    void ReDiscoverPlugins( std::string const& dir );

protected:
    void changeEvent(QEvent *e);
    /// EventFilter that we use to intercept keypresses on list widgets
    bool eventFilter(QObject *obj, QEvent *event);

private:
    Ui::PluginSelectionTab *ui;
    MainWindow* m_mainWindow;

    std::map< QListWidgetItem*, UIPluginInterface* > m_itemInterfaceMap;

    void InstantiatePlugin( QListWidgetItem* item );
    void CreateUIPlugin( const std::string& pluginFactoryName,
                         ves::xplorer::plugin::PluginBase* xplorerPlugin );
    ///On file load populate the plugin tree
    void FileLoadedSlot( const std::string& fileName );

    ///Remove a plugin from xplorer via plugin id
    void RemovePlugin( UIPluginInterface* plugin );

    std::map< QListWidgetItem*, QWidget* > m_itemWidgetMap;

    ves::xplorer::eventmanager::ScopedConnectionList m_connections;

protected Q_SLOTS:
    /// Handles clicks on Add Plugin button
    void on_m_addPluginButton_clicked();
    /// Handles clicks on Remove Plugin button
    void on_m_removePluginButton_clicked();

    /// Handles doubleclicks and pressing "enter" on instantiated plugins list
    void on_m_instantiatedPlugins_itemDoubleClicked( QListWidgetItem* item );

    /// Handles doubleclicks and pressing "enter" on available plugins list
    void on_m_availablePlugins_itemActivated( QListWidgetItem* item );

    /// Called when data of item changes
    void on_m_instantiatedPlugins_itemChanged ( QListWidgetItem * item );
    /// Called when current item selection is changed via keyboard or mouse
    void on_m_instantiatedPlugins_currentItemChanged ( QListWidgetItem * current, QListWidgetItem * previous );

    void qCreateUIPlugin( const std::string& pluginFactoryName,
                          ves::xplorer::plugin::PluginBase* xplorerPlugin );
    void qFileLoadedSlot( const std::string& fileName );

Q_SIGNALS:
    void CreateUIPluginQSignal( const std::string& pluginFactoryName,
                              ves::xplorer::plugin::PluginBase* xplorerPlugin );
    void FileLoadedQSignal( const std::string& fileName );

};

}
}
