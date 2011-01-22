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
#include <ves/conductor/qt/ui_TreeTab.h>

#include <ves/conductor/qt/TreeTab.h>
#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>

#include <osgQtTree/osgQtTree.h>
#include <osgQtTree/treemodel.h>
#include <osgQtTree/osgTreeItem.h>

#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/cad/CADNode.h>

#include <iostream>

Q_DECLARE_METATYPE(osg::NodePath)

namespace ves
{
namespace conductor
{

TreeTab::TreeTab(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::TreeTab)
{
    ui->setupUi(this);

    ////////////// Making a Tree /////////////////////
    // Create a tree model and set the column headers on it
    mModel = new TreeModel;
    QList<QVariant> rootData;
    rootData << "Node Name";
    mModel->CreateRootItem( rootData );

    // Connect the tree model up to a tree view.
    ui->mTreeView->setModel( mModel );
    ui->mTreeView->header()->setResizeMode(QHeaderView::ResizeToContents);

    mBrowser = new PropertyBrowser( this );

    qRegisterMetaType<osg::NodePath>();
    QObject::connect( this, SIGNAL( ObjectPicked( osg::NodePath ) ),
                      this, SLOT( QueuedOnObjectPicked( osg::NodePath ) ),
                      Qt::QueuedConnection );

    // Connect to ObjectPickedSignal so we can update the scenegraph tree view when
    // an object is picked
    CONNECTSIGNALS_1( "%ObjectPickedSignal",
                     void( osg::NodePath& ),
                     &TreeTab::OnObjectPicked,
                     mConnections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
TreeTab::~TreeTab()
{
    delete mBrowser;
    delete mModel;
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::changeEvent(QEvent *e)
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
void TreeTab::Clear()
{
    ui->mTreeView->setModel( 0 );
    delete mModel;
    mModel = new TreeModel;
    ui->mTreeView->setModel( mModel );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::PopulateWithRoot( osg::Node* root )
{
    mModel->BeginReset();
    
    mModel->Clear();
    QList<QVariant> rootData;
    rootData << "Node Name";
    mModel->CreateRootItem( rootData );
    
    // Create a node visitor that will add items into the tree model, and
    // turn it loose on the root node of the scenegraph
    osgQtTree::PopulateTreeControlWithNodeVisitor populator( mModel );
    root->accept( populator );

    mModel->EndReset();
}
////////////////////////////////////////////////////////////////////////////////
QModelIndex TreeTab::OpenToAndSelect( osg::NodePath& nodepath, bool highlight )
{
    // Get the modelindex associated with this nodepath
    QModelIndex result( osgQtTree::openToAndSelect( ui->mTreeView, mModel, nodepath ) );

    on_mTreeView_activated( result, highlight );
    return result;
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_mTreeView_clicked( const QModelIndex& index )
{
    on_mTreeView_activated( index, true );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_mTreeView_activated( const QModelIndex& index, bool highlight )
{
    //Unselect the previously-selected DCS
    if( highlight )
    {
        ves::xplorer::DeviceHandler::instance()->UnselectObjects();
    }

    // Write out the previously-selected CADPropertySet so that changes are not
    // lost
    if( mActiveSet.get() != 0 )
    {
        mActiveSet->WriteToDatabase();
    }

    // Get the node associated with this QModelIndex
    osgQtTree::osgTreeItem* item = static_cast< osgQtTree::osgTreeItem* >( index.internalPointer() );
    osg::Node* node = item->GetNode();

    // Walk up the graph until we find a valid DCS
    bool found = false;
    while( !found )
    {
        osg::Node::DescriptionList descList = node->getDescriptions();
        for( size_t i = 0; i < descList.size(); ++i )
        {
            if( descList.at( i ) == "VE_XML_ID" )
            {
                found = true;
            }
        }
        if( !found )
        {
            if( node->getNumParents() != 0 )
            {
                node = node->getParent( 0 );
            }
            else
            {
                break;
            }
        }
    }

    ves::xplorer::data::PropertySetPtr nullPtr;

    if( !found )
    {
        // Clear out the PropertyBrowser widget
        mBrowser->ParsePropertySet( nullPtr );
        mActiveSet = nullPtr;
        return;
    }

    ves::xplorer::scenegraph::DCS* newSelectedDCS = static_cast< ves::xplorer::scenegraph::DCS* >( node );

    // highlight will be true if this method was called from the tree widget in
    // any form, and false if it was called as a side-effect of
    // ObjectPickedSignal
    if( highlight )
    {
        //Set the selected DCS
        ves::xplorer::DeviceHandler::instance()->SetSelectedDCS( newSelectedDCS );
        newSelectedDCS->SetTechnique( "Glow" );
    }

    // Set mActiveSet null to cause previous propertyset to go out of scope
    mActiveSet = nullPtr;

    // Create a CADPropertySet and load it in the browser
    mActiveSet = ves::xplorer::data::PropertySetPtr( new ves::xplorer::data::CADPropertySet() );
    mActiveSet->SetUUID( newSelectedDCS->GetCADPart()->GetID() );
    mBrowser->ParsePropertySet( mActiveSet );

    ui->cadPropertyBrowser->setPropertyBrowser( mBrowser );
    ui->cadPropertyBrowser->RefreshContents();
    ui->cadPropertyBrowser->show();

    mActiveSet->LoadFromDatabase();
    mBrowser->RefreshAll();
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_RefreshButton_clicked()
{
    if( mActiveSet )
    {
        mActiveSet->LoadFromDatabase();
        mBrowser->RefreshAll();
    }
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_OKButton_clicked()
{
    if( mActiveSet )
    {
        mActiveSet->WriteToDatabase();
//        ves::xplorer::ModelHandler::instance()->GetActiveModel()->
//                GetModelCADHandler()->
//                UpdateCADNode( mActiveSet->GetUUIDAsString() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::OnObjectPicked( osg::NodePath& nodePath )
{
    // emit Qt-signal which is connected to QueuedOnObjectPicked
    // A queued connection is necessary because widgets are altered during
    // this event.
    ObjectPicked( nodePath );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::QueuedOnObjectPicked( osg::NodePath nodePath )
{
    // This is a bit hackish. Instead of re-reading the scenegraph every time a new
    // selection is made, we should be hooked up to signals for changes to the
    // scenegraph so we can re-read at the appropriate time. The only operation
    // that *should* be done here is OpenToAndSelect.
    PopulateWithRoot(
        ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );

    // Open the tree to this node, but don't attempt to highlight the geometry
    // again
    OpenToAndSelect( nodePath, false );
}

} // namespace conductor
} // namespace ves
