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

#include <propertystore/PropertySetPtr.h>
#include <switchwire/ScopedConnectionList.h>
#include <ves/util/SimpleDataTypeSignalSignatures.h>
#include <ves/xplorer/Logging.h>

#include <QtGui/QWidget>
#include <QtCore/QAbstractItemModel>
#include <QtCore/QModelIndex>

#include <osg/Node>

#include <switchwire/Event.h>

namespace Ui
{
class TreeTab;
}

class TreeModel;

/*!\file TreeTab.h
 * Tree Tab
 * \class ves::conductor::TreeTab
 * This class manages the CAD data for the Xplorer Qt window.
 * \namespace ves::conductor
 * UI Namespace
 */

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
}
}
namespace conductor
{

class TreeTab : public QWidget
{
    Q_OBJECT
public:
    ///Constructor
    TreeTab( QWidget* parent = 0 );
    ///Destructor
    ~TreeTab();

    /// Clear the tree and underlying model
    void Clear();

    /// Set the root node of the tree and have it populate the model starting
    /// at this root
    void PopulateWithRoot( osg::Node* root );

    /// Open tree view to the leaf specified in nodepath, if possible.
    QModelIndex OpenToAndSelect( osg::NodePath& nodepath, bool highlight = true );

    std::string GetSelectedNodeID();

protected:
    void changeEvent( QEvent* e );

    // Is connected to KeyboardMouse.ObjectPickedSignal so the
    // tree selection is synchronized with object selection.
    void OnObjectPicked( osg::NodePath& nodePath );

    void Select( const QModelIndex& index, bool highlight );

    /// Syncs db with current transform info read from DCS. This is done
    /// right after a CADPropertySet is read from the db but before its
    /// live properties are enabled.
    void SyncTransformFromDCS( ves::xplorer::scenegraph::DCS* dcs );

    void OnNodeAdded( std::string const& filename );
    void ScenegraphChanged();

    void RefreshTree();

    std::string CreateSubNodePropertySet( osg::Node* node, osg::NodePath& path );

    ///Once we have picked a node on the layers tab this will delete it if
    ///it is a CAD node.
    void DeleteCADNode( osg::Node* node );

    ///Once we have picked a node on the layers tab this will delete it if
    ///it is a Data node.
    void DeleteDataNode( osg::Node* node );
    
protected Q_SLOTS:
    /// Called when user changes selection in tree.
    /// This function looks up the scenegraph to find a valid DCS and then
    /// selects the corresponding geometry in the scene.
    void on_mTreeView_activated( const QModelIndex& index );

    /// Called when mouse is used to select an entry in the tree. Simply calls
    /// on_mTreeView_activated.
    void on_mTreeView_clicked( const QModelIndex& index );

    /// Reloads the current CADPropertySet from the DB.
    void on_RefreshButton_clicked();

    /// Writes the current CADPropertySet to the DB.
    void on_OKButton_clicked();

    ///Tells the graphics side to frame all on the selected node
    void on_m_navToButton_clicked();

    /// Slot corresponding to ObjectPicked queued signal. The final destination
    /// of logic begun in slot OnObjectPicked.
    void QueuedOnObjectPicked( osg::NodePath nodePath );
    /// Slot corresponding to NodeAddedQSignal. Final destination of logic begun
    /// in NodeAdded.
    void QueuedNodeAdded( std::string const& filename );

    //void on_m_refreshTreeButton_clicked();

    void on_m_searchBox_textEdited( const QString& pattern );

    void on_m_deleteButton_clicked();

    void on_m_expandAllButton_clicked();

Q_SIGNALS:

    /// Queued signal emitted when OnObjectPicked slot is called. This is
    /// required for thread safety
    void ObjectPicked( osg::NodePath nodePath );
    /// Queued signal emitted when NodeAdded slot is called. This is
    /// required for thread safety
    void NodeAddedQSignal( std::string const& filename );

private:
    ///The underlying qt ui object
    Ui::TreeTab* ui;
    ///The layers tree
    TreeModel* mModel;
    ///Active pset
    propertystore::PropertySetPtr mActiveSet;
    ///Switchwire list of signals for this class
    switchwire::ScopedConnectionList mConnections;

    ///Highlight and add manipulators
    switchwire::Event< void( osg::NodePath& ) > m_highlightAndSetManipulators;
    ///Add a glow to the selected  node
    switchwire::Event< void( osg::NodePath& ) > m_highlightNode;
    ///Signal to trigger the nav to animation
    switchwire::Event< void( osg::NodePath const& ) > m_navToNode;
    ///Select a given node
    ves::util::StringSignal_type m_CADNodeSelected;

    ///Logger reference
    Poco::Logger& m_logger;
    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;
};

} // namespace conductor
} // namespace ves
