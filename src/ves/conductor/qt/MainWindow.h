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

#include <QtGui/QMainWindow>
#include <QtGui/QFileDialog>

//Needed for NodePath
#include <osg/Node>

#include <ves/xplorer/eventmanager/ScopedConnectionList.h>

#include <ves/open/xml/model/SystemPtr.h>

#include <ves/VEConfig.h>

#include <string>
#include <map>


namespace ves 
{ 
namespace conductor 
{ 
class PreferencesTab;
class TreeTab;
class IconStack;
class PluginSelectionTab;

namespace qt 
{ 
namespace minerva 
{ 
class StackedWidget; 
} 
} 
} 
}

namespace Ui 
{
class MainWindow;
} // Ui

/*!\file MainWindow.h
 Main Qt Window
 */
/*!\class ves::conductor::MainWindow
 * This class manages the main Qt window.
 */
/*!\namespace ves::conductor
 * UI Namespace
 */

namespace ves
{
namespace conductor
{
class VE_CONDUCTOR_QTUI_EXPORTS MainWindow : public QMainWindow 
{
    Q_OBJECT
public:
    MainWindow(QWidget* parent = 0);
    virtual ~MainWindow();
    
    /// Adds @c widget to tabs and gives tab the label @c tabLabel.
    int AddTab( QWidget* widget, const std::string& tabLabel );
    
    /// Remove tab containing @c widget. Does not delete the widget.
    void RemoveTab( QWidget* widget );
    
    /// Remove tab with label @c tabLabel. Does not delete the associated widget.
    void RemoveTab( const std::string& tabLabel );
    
    /// Remove all existing tabs. Does not delete the underlying widgets.
    void RemoveAllTabs();
    
    /// Activate tab containing @c widget
    void ActivateTab( QWidget* widget );
    
    /// Activate tab with label @c tabLabel
    void ActivateTab( const std::string& tabLabel );
    
    /// Activate tab at index @c index
    void ActivateTab( int index );

protected:
    void changeEvent(QEvent* e);
    
    /// Is connected to ModelHandler.ActiveModelChangedSignal so that
    /// appropriate tabs can be shown when the active model changes.
    void OnActiveModelChanged( const std::string& modelID );

protected Q_SLOTS:
    
    /// Called when the physics icon is pressed on the toolbar
    /// Autoconnected slot
    void on_actionPhysicsStack_triggered(); 
    
    /// Enables physics engine
    /// Autoconnected slot
    void on_actionStepPhysics_triggered(); 
    
    ///Start the physics engine
    /// Autoconnected slot
    void on_actionPlayPhysics_triggered(); 
    
    ///Pause he physics engine
    /// Autoconnected slot
    void on_actionPausePhysics_triggered(); 
    
    ///Reset the physics engine
    /// Autoconnected slot
    void on_actionResetPhysics_triggered(); 
    
    ///Debug physics
    /// Autoconnected slot
    //void on_actionDebugPhysics_triggered();

    /// Called when the manipulators icon is pressed on the toolbar
    /// Autoconnected slot
    void on_actionManipulatorStack_triggered(); 
    
    /// Called when the manipulators icon is pressed on the toolbar
    /// Autoconnected slot
    //void on_actionManipulatorStack_hovered();

    /// Enables physics engine
    /// Autoconnected slot
    void on_actionScaleManipulator_triggered(); 
    
    ///Start the physics engine
    /// Autoconnected slot
    void on_actionTranslateManipulator_triggered(); 
    
    ///Pause he physics engine
    /// Autoconnected slot
    void on_actionRotateManipulator_triggered(); 
    
    ///Reset the physics engine
    /// Autoconnected slot
    void on_actionComboManipulator_triggered(); 

    ///Reset the physics engine
    /// Autoconnected slot
    void on_actionEnableManipulator_triggered( bool triggered ); 
    
    /// Called when the file operations icon on the main toolbar is clicked.
    /// Autoconnected slot
    void on_actionFile_triggered(); 
    
    /// Called when the open file icon of the file operations stack is clicked.
    /// Autoconnected slot
    void on_actionOpen_triggered(); 

    /// Called when the file save icon of the file operations stack is clicked.
    /// Autoconnected slot.
    void on_actionSave_triggered();
    
    /// Called when a user selects quit.
    void on_actionQuit_triggered();

    /// Called when user selects a filter in the open file tab. This gives
    /// us the opportunity to allow or disallow multiple selection based on
    /// file type.
    void OnOpenFileFilterSelected ( const QString& filter );

    /// Called when user clicks a filename in file open tab. This lets us
    /// allow or disallow multiple selection based on the extension of the
    /// selected file.
    void OnCurrentChanged ( const QString& path );
    
    /// Called when a valid file selection is made via the file open dialog.
    void onFileOpenSelected( const QStringList& fileNames );
    
    /// Called when the file selection dialog has been cancelled
    void onFileCancelled();

    /// Called when a filename has been chosen for saving.
    void onFileSaveSelected( const QString& fileName );
    
    /// Called when current tab is changed.
    /// Autoconnected slot
    void on_tabWidget_currentChanged( int index );


    /// Called when the manipulators icon is pressed on the toolbar
    /// Autoconnected slot
    void on_actionNavigationStack_triggered(); 
    
    /// Called when the manipulators icon is pressed on the toolbar
    /// Autoconnected slot
    void on_actionSmallJump_hovered();
    
    /// Enables physics engine
    /// Autoconnected slot
    void on_actionMediumJump_triggered(); 
    
    ///Start the physics engine
    /// Autoconnected slot
    void on_actionLargeJump_triggered(); 
    
    ///Pause he physics engine
    /// Autoconnected slot
    void on_actionBoundingBoxJump_triggered(); 
    
    ///Reset the physics engine
    /// Autoconnected slot
    void on_actionWorldNavigation_triggered(); 

    ///Reset the physics engine
    /// Autoconnected slot
    void on_actionObjectNavigation_triggered(); 
    
    ///Control character navigation
    /// Autoconnected slot
    void on_actionCharacterNavigation_triggered( bool triggered ); 

    ///Control character fly mode
    /// Autoconnected slot
    void on_actionCharacterFlyMode_triggered( bool triggered ); 

    void on_actionViewStack_triggered( );

    void on_actionShowPluginsTab_triggered();

    void on_actionShowPreferencesTab_triggered();

    // Something in the qt slots mechanism breaks down if these are surrounded by a #ifndef/#endif block.
	  void on_actionAdd_Planet_triggered ( bool );
	  void on_actionRemove_Planet_triggered ( bool );
	  void on_actionConfigure_Layers_triggered ( bool );

Q_SIGNALS:
    /// Queued signal emitted when OnActiveModelChanged slot is called. This is
    /// required for thread safety
    void ActiveModelChanged( std::string modelID );

protected Q_SLOTS:
    /// Slot corresponding to ActiveModelChanged queued signal
    void QueuedOnActiveModelChanged( const std::string& modelID );

private:
    ///Qt window widget
    Ui::MainWindow* ui;
    ///Q file dialog
    QFileDialog* mFileDialog;
    ///File menu stack
    ves::conductor::IconStack* mFileOpsStack;
    ///Physics menu stack
    ves::conductor::IconStack* m_physicsMenuStack;
    ///Physics menu stack
    ves::conductor::IconStack* m_manipulatorMenuStack;
    ///Nav menu stack
    ves::conductor::IconStack* m_navMenuStack;
    ///View menu stack
    ves::conductor::IconStack* m_viewMenuStack;
    ///Tree tab
    ves::conductor::TreeTab* mScenegraphTreeTab;
    ///The map of tabs
    std::map< std::string, QWidget* > mTabbedWidgets;
    ///Active tab key
    std::string mActiveTab;
    
    /// Maintains the list of signals this object is connected to
    ves::xplorer::eventmanager::ScopedConnectionList mConnections;
    
    // Main tabs that this class owns and manages
    QWidget* mVisualizationTab;

    ///The layers tree pointer
    ves::conductor::qt::minerva::StackedWidget* mMinervaStackedWidget;
    
    ///The PreferencesTab pointer
    ves::conductor::PreferencesTab* m_preferencesTab;

    ves::conductor::PluginSelectionTab* m_pluginsTab;
    
    /// ButtonPress signal type
    /// Params are: button, x, y, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< void ( const std::string ) > NavJumpSignal_type;
    NavJumpSignal_type m_jumpSignal;  

    boost::signals2::signal< void () > m_resyncFromDB;

    void SaveSytemToFile( ves::open::xml::model::SystemPtr system, std::string fileName );
};
    
}
}
