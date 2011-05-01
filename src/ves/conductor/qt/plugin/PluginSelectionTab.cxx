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

#include <ves/xplorer/eventmanager/EventManager.h>

// This is temporary until I get this sorted out. moc needs to have the include path
// set so it can find these things in the correct place.
#include <ves/conductor/qt/DefaultPlugin/UIPluginFactory.h>
#include <ves/conductor/qt/DefaultPlugin/UIPluginInterface.h>
#include <ves/conductor/qt/plugin/UIPluginBase.h>
#include <ves/conductor/qt/plugin/ui_PluginPorts.h>

#include <ves/conductor/qt/XMLDataBufferEngine.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/model/Network.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/network/GraphicalPluginManager.h>
#include <ves/xplorer/plugin/PluginBase.h>

#include <iostream>
#include <sstream>

#include <QtCore/QPluginLoader>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QDir>
#include <QtCore/QStringList>
#include <QtGui/QIcon>
#include <QtGui/QLabel>

Q_DECLARE_METATYPE(std::string)
Q_DECLARE_METATYPE(ves::xplorer::plugin::PluginBase)

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
PluginSelectionTab::PluginSelectionTab( MainWindow* mainWindow, QWidget *parent ) :
    QWidget(parent),
    ui(new Ui::PluginSelectionTab),
    m_mainWindow( mainWindow )
{
    ui->setupUi(this);

    QString qPluginDir = 
        QProcessEnvironment::systemEnvironment().value ( "CONDUCTOR_PLUGINS_DIR" );
    std::string pluginsDir;
    if( qPluginDir.toStdString().empty() )
    {
        std::cout << "|\tCONDUCTOR_PLUGINS_DIR is not defined." 
            << std::endl << std::flush;
        qPluginDir = 
            QProcessEnvironment::systemEnvironment().value ( "XPLORER_PLUGINS_DIR" );
        pluginsDir = qPluginDir.toStdString() + "/../../conductor/plugins";
        std::cout << "|\tUsing " << pluginsDir << " instead." 
            << std::endl << std::flush;
    }
    else
    {
        std::cout << "|\tCONDUCTOR_PLUGINS_DIR = " 
            << qPluginDir.toStdString() << std::endl << std::flush;
        pluginsDir = qPluginDir.toStdString();
    }

    //QDir pluginsPath = qApp->applicationDirPath();
    //pluginsPath.cd("conductor/plugins");
    QString tempDir( pluginsDir.c_str() );
    QDir pluginsPath( tempDir );
    
    std::cout << "|\tConductor is searching for plugins in " 
        << pluginsPath.canonicalPath().toStdString() << std::endl << std::flush;

    // Walk through all files in Plugins/UI directory
    QStringList files = pluginsPath.entryList(QDir::Files);
    QStringList::iterator iter = files.begin();
    if( iter == files.end() )
    {
        std::cout << "|\tConductor found no plugins." 
            << std::endl << std::flush;
    }


    while( iter != files.end() )
    {
        QString fileName = (*iter);
        QPluginLoader loader;
        loader.setFileName( pluginsPath.absoluteFilePath(fileName) );
        std::cout << "|\tChecking whether " << fileName.toStdString()
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
                    std::cout << "|\tConductor successfully loaded plugin " << fileName.toStdString()
                            << " containing plugin: " << factory->GetFactoryName() << "-- "
                            << factory->GetDescription() << std::endl << std::flush;
                    QString name;
                    QListWidgetItem* item = new QListWidgetItem( factory->GetIcon(), name.fromStdString( factory->GetFactoryName() ), ui->m_availablePlugins );
                    // Set the plugin's full path as extra data in the item so that later
                    // we can create new instances of this plugin.
                    item->setData( Qt::UserRole, loader.fileName() );
                    //item->setFlags( item->flags() | Qt::ItemIsEditable );
                    ui->m_availablePlugins->addItem( item );
                }
                else
                {
                    std::cout << "|\tFailed to cast plugin " << fileName.toStdString() <<
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
    qRegisterMetaType<std::string>();
    qRegisterMetaType<ves::xplorer::plugin::PluginBase>();

    connect( this, SIGNAL( CreateUIPluginQSignal( const std::string& ,
                                         ves::xplorer::plugin::PluginBase* ) ),
                              this, SLOT( qCreateUIPlugin( const std::string& ,
                                         ves::xplorer::plugin::PluginBase*  ) ),
                              Qt::QueuedConnection );

    connect( this, SIGNAL( FileLoadedQSignal( const std::string&  ) ),
                              this, SLOT( qFileLoadedSlot( const std::string& ) ),
                              Qt::QueuedConnection );

    CONNECTSIGNALS_2( "%CreatePlugin",
                      void ( const std::string&, ves::xplorer::plugin::PluginBase* ),
                      &PluginSelectionTab::CreateUIPlugin, m_connections,
                      any_SignalType, normal_Priority);

    CONNECTSIGNALS_1( "%VesFileLoaded%",
                      void( const std::string& ),
                      &PluginSelectionTab::FileLoadedSlot,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
PluginSelectionTab::~PluginSelectionTab()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::on_m_addPluginButton_clicked()
{
    InstantiatePlugin( ui->m_availablePlugins->currentItem() );
}
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::InstantiatePlugin( QListWidgetItem* item )
{
    if( !item )
    {
        return;
    }
    // Get plugin filename from the current item
    QString fileName = item->data( Qt::UserRole ).toString();

    QPluginLoader loader( fileName );
    QObject *plugin = loader.instance();
    if( !plugin )
    {
        return;
    }
    UIPluginFactory* factory =
            qobject_cast< UIPluginFactory* >(plugin);
    if( factory )
    {
        // We have a valid plugin. Add this to the current system, and
        // send to xplorer to load. When xplorer instantiates this plugin,
        // it will send a signal which we catch in InstantiatePluginSlot.
        // Then we finish the process of creating the UI portion of
        // the plugin.
        XMLDataBufferEngine* mDataBufferEngine = XMLDataBufferEngine::instance();

        ///Initialize top level network
        ves::open::xml::model::NetworkPtr tempNetwork( new ves::open::xml::model::Network() );

        mDataBufferEngine->GetXMLSystemDataObject(
            mDataBufferEngine->GetTopSystemId() )->AddNetwork( tempNetwork );

        using namespace ves::open::xml::model;

        SystemPtr system( mDataBufferEngine->GetXMLSystemDataObject(
                mDataBufferEngine->GetTopSystemId() ) );

        ModelPtr mod( new Model );
        mod->SetPluginType( factory->GetFactoryName() );
        mod->SetPluginName( factory->GetFactoryName() );
        //mod->SetVendorName( "DefaultPlugin" );
        mod->SetParentSystem( system );

        system->AddModel( mod );

        const std::string network = XMLDataBufferEngine::instance()->
                        SaveVESData( std::string( "returnString" ) );

        ves::xplorer::network::GraphicalPluginManager::instance()->SetCurrentNetwork( network );

        ves::xplorer::network::GraphicalPluginManager::instance()->LoadDataFromCE();

        ves::xplorer::DeviceHandler::instance()->SetActiveDCS(
            ves::xplorer::scenegraph::SceneManager::instance()->GetActiveNavSwitchNode() );

        ves::xplorer::ModelHandler::instance()->SetActiveModel( mod->GetID() );
    }
}
////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::on_m_availablePlugins_itemDoubleClicked( QListWidgetItem* item )
{
    InstantiatePlugin( item );
}
////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::CreateUIPlugin( const std::string& pluginFactoryName,
                                         ves::xplorer::plugin::PluginBase* xplorerPlugin)
{
    CreateUIPluginQSignal( pluginFactoryName, xplorerPlugin );
}
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::on_m_instantiatedPlugins_currentItemChanged
     ( QListWidgetItem* current, QListWidgetItem* previous )
{
    boost::ignore_unused_variable_warning( previous );
    if( current )
    {
        std::map< QListWidgetItem*, UIPluginInterface* >::const_iterator iter =
                m_itemInterfaceMap.find( current );
        if( iter != m_itemInterfaceMap.end() )
        {
            UIPluginBase* plugin = dynamic_cast< UIPluginBase* >( iter->second );
            ves::xplorer::ModelHandler::instance()->SetActiveModel(
                    plugin->GetVEModel()->GetID() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::ClearActivePlugins()
{
    std::cout << "|\tClearing active plugins" << std::endl << std::flush;
    std::map< QListWidgetItem*, UIPluginInterface* >::iterator iter =
            m_itemInterfaceMap.begin();
    while( iter != m_itemInterfaceMap.end() )
    {
        delete iter->second;
        ++iter;
    }
    m_itemInterfaceMap.clear();
    ui->m_instantiatedPlugins->clear();
}
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::FileLoadedSlot( const std::string& fileName )
{
    FileLoadedQSignal( fileName );
}
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::qCreateUIPlugin( const std::string& pluginFactoryName,
                      ves::xplorer::plugin::PluginBase* xplorerPlugin )
{
    QString pluginName;
    pluginName = pluginName.fromStdString( pluginFactoryName );

    // See if this plugin exists in the available UI plugins list
    QList<QListWidgetItem *> results = ui->m_availablePlugins->findItems( pluginName, Qt::MatchFixedString );
    if( results.empty() )
    {
        std::cerr << "ERROR -- PluginSelectionTab::InstantiatePlugin: No available UI plugin named "
                << pluginFactoryName << std::endl << std::flush;
        return;
    }
    else
    {
        std::cout << "|Found UI plugin lib matching name " 
            << pluginFactoryName << std::endl << std::flush;
    }

    // Create an instance of the plugin from its factory
    QListWidgetItem* item = results.at(0);
    if( !item )
    {
        return;
    }

    // Get plugin filename from the current item
    QString fileName = item->data( Qt::UserRole ).toString();

    QPluginLoader loader( fileName );
    QObject *plugin = loader.instance();
    if( !plugin )
    {
        return;
    }
    std::cout << "|\tPlugin instance valid" << std::endl << std::flush;
    UIPluginFactory* factory =
            qobject_cast< UIPluginFactory* >(plugin);
    if( factory )
    {
        std::cout << "|\tFactory instance valid" << std::endl << std::flush;
        // Create new instance of the UIPluginInterface object
        // this factory contains.
        UIPluginInterface* interface = factory->CreateInstance();

        // Set its name as interfaceName_index
        int index = ui->m_instantiatedPlugins->count();
        std::stringstream nameSS;
        nameSS << interface->GetName();
        nameSS << "_";
        nameSS << index;
        interface->SetName( nameSS.str() );

        std::cout << "|\tSetting interface name to " 
            << nameSS.str() << std::endl << std::flush;

        // Give the UI plugin a pointer to the xplorer plugin. This
        // will give the UI plugin access to things like the correct model.
        UIPluginBase* base = dynamic_cast<UIPluginBase*>( interface );
        base->SetXplorerPlugin( xplorerPlugin );

        // Use the plugin info to create a new item for the instantiated
        // plugins list. Notice here we use the name of the instance,
        // rather than the name of the factory.
        QString name;
        QListWidgetItem* newItem =
            new QListWidgetItem( 
                factory->GetIcon(), name.fromStdString( interface->GetName() ),
                ui->m_instantiatedPlugins );

        if( newItem )
        {
            std::cout << "|\tCreated item named " 
                << interface->GetName() << std::endl << std::flush;
        }
        else
        {
            std::cout << "|\tFailed to create list item!" 
                << std::endl << std::flush;
        }

        newItem->setFlags( newItem->flags() | Qt::ItemIsEditable );
        ui->m_instantiatedPlugins->addItem( newItem );

        // Store the new interface
        m_itemInterfaceMap[ newItem ] = interface;
        std::cout << "|\tMap size: " << m_itemInterfaceMap.size() 
            << std::endl << std::flush;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PluginSelectionTab::qFileLoadedSlot( const std::string& fileName )
{
    boost::ignore_unused_variable_warning( fileName );
    ClearActivePlugins();
}
////////////////////////////////////////////////////////////////////////////////
}
}
