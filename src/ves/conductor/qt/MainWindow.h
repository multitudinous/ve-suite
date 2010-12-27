/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#include <ves/conductor/qt/IconStack.h>
#include <ves/conductor/qt/TreeTab.h>

#include <ves/xplorer/eventmanager/ScopedConnectionList.h>

#include <ves/VEConfig.h>

namespace ves { namespace conductor { namespace qt { namespace minerva { class LayersTree; } } } }

namespace Ui 
{
    class MainWindow;
} // Ui

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
    
    // Is connected to KeyboardMouse.ObjectPickedSignal so the scenegraph
    // tree selection is synchronized with object selection.
    void OnObjectPicked( osg::NodePath& nodePath );

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

    /// Called when the physics icon is pressed on the toolbar
    /// Autoconnected slot
    void on_actionManipulatorStack_triggered(); 
    
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
    void on_actionEnableManipulator_triggered(); 
    
    /// Called when the file operations icon on the main toolbar is clicked.
    /// Autoconnected slot
    void on_actionFile_triggered(); 
    
    /// Called when the open file icon of the file operations stack is clicked.
    /// Autoconnected slot
    void on_actionOpen_triggered(); 
    
    /// Called when a user selects quit.
    void on_actionQuit_triggered();
    
    /// Called when a valid file selection is made via the file open dialog.
    void onFileOpenSelected( QString fileName );
    
    /// Called when the file selection dialog has been cancelled
    void onFileCancelled();
    
    /// Called when current tab is changed.
    /// Autoconnected slot
    void on_tabWidget_currentChanged( int index );

    Q_SIGNALS:
    /// Queued signal emitted when OnActiveModelChanged slot is called. This is
    /// required for thread safety
    void ActiveModelChanged( std::string modelID );

    /// Queued signal emitted when OnObjectPicked slot is called. This is
    /// required for thread safety
    void ObjectPicked( osg::NodePath nodePath );

protected Q_SLOTS:
    /// Slot corresponding to ActiveModelChanged queued signal
    void QueuedOnActiveModelChanged( std::string modelID );

    /// Slot corresponding to ObjectPicked queued signal
    void QueuedOnObjectPicked( osg::NodePath nodePath );
#ifdef MINERVA_GIS_SUPPORT

private Q_SLOTS:

	void on_actionAdd_Planet_triggered ( bool );
	void on_actionRemove_Planet_triggered ( bool );
	void on_actionConfigure_Layers_triggered ( bool );
#endif
private:
    ///Qt window widget
    Ui::MainWindow* ui;
    ///Q file dialog
    QFileDialog* mFileDialog;
    ///File menu stack
    IconStack* mFileOpsStack;
    ///Physics menu stack
    IconStack* m_physicsMenuStack;
    ///Physics menu stack
    IconStack* m_manipulatorMenuStack;
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

    ves::conductor::qt::minerva::LayersTree *mLayersTree;
};
