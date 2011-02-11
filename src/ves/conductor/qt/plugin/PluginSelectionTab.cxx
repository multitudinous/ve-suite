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
#include <ves/conductor/qt/plugin/ui_PluginSelectionTab.h>
#include <ves/conductor/qt/plugin/PluginSelectionTab.h>
#include <ves/conductor/qt/MainWindow.h>


// This is temporary until I get this sorted out. moc needs to have the include path
// set so it can find these things in the correct place.
#include <ves/conductor/qt/DefaultPlugin/UIPluginFactory.h>
#include <ves/conductor/qt/DefaultPlugin/UIPluginInterface.h>
#include <ves/conductor/qt/plugin/UIPluginBase.h>
#include <ves/conductor/qt/plugin/ui_PluginPorts.h>

#include <iostream>
#include <sstream>


#include <QtCore/QPluginLoader>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QDir>
#include <QtCore/QStringList>
#include <QtGui/QIcon>
#include <QtGui/QLabel>

namespace ves
{
namespace conductor
{

PluginSelectionTab::PluginSelectionTab( MainWindow* mainWindow, QWidget *parent ) :
    QWidget(parent),
    ui(new Ui::PluginSelectionTab),
    m_mainWindow( mainWindow )
{
    ui->setupUi(this);

//    QString pluginDir = QProcessEnvironment::systemEnvironment().value ( "CONDUCTOR_PLUGINS_DIR" );
//    std::cout << "$CONDUCTOR_PLUGINS_DIR = " << pluginDir.toStdString() << std::endl << std::flush;

    QDir pluginsPath = qApp->applicationDirPath();
    pluginsPath.cd("Plugins/UI");

    std::cout << "Searching for plugins in " << pluginsPath.canonicalPath().toStdString() << std::endl << std::flush;

    // Walk through all files in Plugins/UI directory
    QStringList files = pluginsPath.entryList(QDir::Files);
    QStringList::iterator iter = files.begin();
    if( iter == files.end() )
    {
        std::cout << "No plugins found." << std::endl << std::flush;
    }


    while( iter != files.end() )
    {
        QString fileName = (*iter);
        QPluginLoader loader;
        loader.setFileName( pluginsPath.absoluteFilePath(fileName) );
        std::cout << "Checking whether " << fileName.toStdString()
                << " is a plugin...";
        if( loader.load() )
        {
            std::cout << "yes." << std::endl << std::flush;

            // Get root object, which should be a UIPluginFactory
            QObject *plugin = loader.instance();
            if (plugin)
            {
                UIPluginFactory* factory = qobject_cast< UIPluginFactory* >(plugin);
                if( factory )
                {
                    std::cout << "Successfully loaded plugin " << fileName.toStdString()
                            << " containing plugin: " << factory->GetFactoryName() << "-- "
                            << factory->GetDescription() << std::endl << std::flush;
                    QString name;
                    QListWidgetItem* item = new QListWidgetItem( factory->GetIcon(), name.fromStdString( factory->GetFactoryName() ), ui->m_availablePlugins );
                    // Set the plugin's full path as extra data in the item so that later
                    // we can create new instances of this plugin.
                    item->setData( Qt::UserRole, loader.fileName() );
                    item->setFlags( item->flags() | Qt::ItemIsEditable );
                    ui->m_availablePlugins->addItem( item );
                }
                else
                {
                    std::cout << "Failed to cast plugin " << fileName.toStdString() <<
                            " as a UIPluginFactory." << std::endl << std::flush;
                }
            }
            else
            {
                std::cout << loader.errorString().toStdString() << std::endl << std::flush;
            }
        }
        else
        {
            std::cout << "no. " << loader.errorString().toStdString() << std::endl << std::flush;
        }
        ++iter;
    }
}

PluginSelectionTab::~PluginSelectionTab()
{
    delete ui;
}

void PluginSelectionTab::changeEvent(QEvent *e)
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

void PluginSelectionTab::on_m_addPluginButton_clicked()
{
    InstantiatePlugin( ui->m_availablePlugins->currentItem() );
}

void PluginSelectionTab::InstantiatePlugin( QListWidgetItem* item )
{
    if( item )
    {
        // Get plugin filename from the current item
        QString fileName = item->data( Qt::UserRole ).toString();

        QPluginLoader loader( fileName );
        QObject *plugin = loader.instance();
        if (plugin)
        {
            UIPluginFactory* factory =
                    qobject_cast< UIPluginFactory* >(plugin);
            if( factory )
            {
                // Create new instance of the UIPluginInterface object
                // this factory contains.
                UIPluginInterface* interface = factory->CreateInstance();


                int index = ui->m_instantiatedPlugins->count();
                std::stringstream nameSS;
                nameSS << interface->GetName();
                nameSS << "_";
                nameSS << index;
                interface->SetName( nameSS.str() );

                // Use the plugin info to create a new item for the instantiated
                // plugins list. Notice here we use the name of the instance,
                // rather than the name of the factory.
                QString name;
                QListWidgetItem* newItem =
                        new QListWidgetItem( factory->GetIcon(),
                                             name.fromStdString( interface->
                                                                 GetName() ),
                                             ui->m_instantiatedPlugins );

                newItem->setFlags( newItem->flags() | Qt::ItemIsEditable );
                ui->m_instantiatedPlugins->addItem( newItem );

                // Store the new interface
                m_itemInterfaceMap[ newItem ] = interface;
            }
        }
    }
}

void PluginSelectionTab::on_m_instantiatedPlugins_itemDoubleClicked( QListWidgetItem* item )
{
    // Check whether we've already composed a widget and tab for this item to
    // prevent showing multiple tabs
    std::map< QListWidgetItem*, QWidget* >::iterator witer =
            m_itemWidgetMap.find( item );
    if( witer != m_itemWidgetMap.end() )
    {
        // Just activate the tab.
        m_mainWindow->ActivateTab( witer->second );
        return;
    }

    std::map< QListWidgetItem*, UIPluginInterface* >::const_iterator iter =
            m_itemInterfaceMap.find( item );
    if( iter != m_itemInterfaceMap.end() )
    {
        UIPluginInterface* interface = iter->second;

        // Compose a widget containing the PluginPorts ui in the top part and
        // the UIWidget associated with this plugin in the bottom part
        Ui::Form* qtop = new Ui::Form;
        QWidget* top = new QWidget(0);
        qtop->setupUi( top );
        QWidget* bottom = interface->GetUIWidget();
        QVBoxLayout* layout = new QVBoxLayout;
        layout->addWidget(top);
        layout->addWidget(bottom);
        QWidget* tempWidget = new QWidget(0);
        tempWidget->setLayout( layout );

        UIPluginBase* pb = dynamic_cast<UIPluginBase*>(interface);
        qtop->m_inputPortsSpinner->setValue( pb->GetNumInPorts() );
        qtop->m_outputPortsSpinner->setValue( pb->GetNumOutPorts() );

        connect( qtop->m_inputPortsSpinner, SIGNAL(valueChanged(int)), pb, SLOT(SetNumInputPorts(int)));
        connect( qtop->m_outputPortsSpinner, SIGNAL(valueChanged(int)), pb, SLOT(SetNumOutputPorts(int)));

        // Add the composed widget as a tab
        m_mainWindow->ActivateTab(
                m_mainWindow->AddTab( tempWidget, interface->GetName() ) );

        // Keep track of the composed widget so it can be deleted at the right
        // time
        m_itemWidgetMap[item] = tempWidget;
    }
}

void PluginSelectionTab::on_m_availablePlugins_itemDoubleClicked( QListWidgetItem* item )
{
    InstantiatePlugin( item );
}

void PluginSelectionTab::on_m_instantiatedPlugins_itemChanged ( QListWidgetItem * item )
{   
    std::map< QListWidgetItem*, UIPluginInterface* >::const_iterator iter =
            m_itemInterfaceMap.find( item );
    if( iter != m_itemInterfaceMap.end() )
    {
        UIPluginInterface* interface = iter->second;
        interface->SetName( item->text().toStdString() );

        // Re-add the tab, which will cause the tab name to be updated.
        m_mainWindow->ActivateTab(
                m_mainWindow->AddTab( m_itemWidgetMap[ item ], interface->GetName() ) );
    }
}

void PluginSelectionTab::on_m_removePluginButton_clicked()
{
    QListWidgetItem* item = ui->m_instantiatedPlugins->currentItem();

    if( item )
    {
        std::map< QListWidgetItem*, UIPluginInterface* >::iterator iter =
                m_itemInterfaceMap.find( item );
        if( iter != m_itemInterfaceMap.end() )
        {
            // Deleting the widgets will automatically remove the tabs
            iter->second->DeleteWidgets();
            delete iter->second;
            m_itemInterfaceMap.erase( iter );
        }

        std::map< QListWidgetItem*, QWidget* >::iterator witer =
                m_itemWidgetMap.find( item );
        if( witer != m_itemWidgetMap.end() )
        {
            delete witer->second;
            m_itemWidgetMap.erase( witer );
        }

        // Remove entry in instantiated plugins ListView
        delete ui->m_instantiatedPlugins->takeItem( ui->m_instantiatedPlugins->row( item ) );
    }
}

}
}
