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
#include <ves/conductor/qt/TreeTab.h>

#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>

#include <osgQtTree/osgQtTree.h>
#include <osgQtTree/treemodel.h>
#include <osgQtTree/osgTreeItem.h>

#include <QtGui/QScrollBar>

#include <ves/conductor/qt/ui_TreeTab.h>

#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

#include <ves/open/xml/cad/CADNode.h>

#include <osgwTools/NodePathUtils.h>

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

    ves::xplorer::eventmanager::EventManager::instance()->RegisterSignal(
            new ves::xplorer::eventmanager::SignalWrapper<
            boost::signals2::signal< void( osg::NodePath& ) > >( &m_highlightAndSetManipulators ),
        "TreeTab.HighlightAndSetManipulators" );
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
    //Don't collapse tree if a null selection has been made.
    if( nodepath != osg::NodePath() )
    {
        //ui->mTreeView->collapseAll();
    }

    // Get the modelindex associated with this nodepath
    QModelIndex result( osgQtTree::openToAndSelect( ui->mTreeView, mModel, nodepath ) );

    // Scroll horizontally to the maximum. Since this comes after a call to
    // collapseAll, followed by a call to openToAndSelect, which expands to the
    // selected item, the view should be in minimally expanded
    // state, meaning scrolling horizontally all the way will make the current
    // item visible horizontally.
    int max = ui->mTreeView->horizontalScrollBar()->maximum();
    ui->mTreeView->horizontalScrollBar()->setValue( max );

    Select( result, highlight );
    return result;
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_mTreeView_clicked( const QModelIndex& index )
{
    on_mTreeView_activated( index );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_mTreeView_activated( const QModelIndex& index )
{
    Select( index, true );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::Select( const QModelIndex& index, bool highlight )
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
    osg::Node* node = 0;
    if( index != QModelIndex() )
    {
        osgQtTree::osgTreeItem* item = static_cast< osgQtTree::osgTreeItem* >( index.internalPointer() );
        node = item->GetNode();
    }

    // See if this node has a VE_XML_ID
    bool found = false;
    if( node )
    {
        osg::Node::DescriptionList descList = node->getDescriptions();
        for( size_t i = 0; i < descList.size(); ++i )
        {
            if( descList.at( i ) == "VE_XML_ID" )
            {
                found = true;
            }
        }
    }

    if( !found )
    {
        // Clear out the PropertyBrowser widget
        ves::xplorer::data::PropertySetPtr nullPtr;
        mBrowser->ParsePropertySet( nullPtr );
        mActiveSet = nullPtr;
        return;
    }

    ves::xplorer::scenegraph::DCS* newSelectedDCS = static_cast< ves::xplorer::scenegraph::DCS* >( node );

    // Create a CADPropertySet and load it in the browser
    mActiveSet = ves::xplorer::data::PropertySetPtr( new ves::xplorer::data::CADPropertySet() );
    mActiveSet->SetUUID( newSelectedDCS->GetCADPart()->GetID() );
    mBrowser->ParsePropertySet( mActiveSet );

    ui->cadPropertyBrowser->setPropertyBrowser( mBrowser );
    ui->cadPropertyBrowser->RefreshContents();
    ui->cadPropertyBrowser->show();

    // Load properties from db
    mActiveSet->LoadFromDatabase();
    // Update transform properties to agree with the current state of the
    // associated DCS
    SyncTransformFromDCS();
    // Turn on live updates
    static_cast<ves::xplorer::data::CADPropertySet*>(mActiveSet.get())->
            EnableLiveProperties( true );

    mBrowser->RefreshAll();

    // highlight will be true if this method was called from the tree widget in
    // any form, and false if it was called as a side-effect of
    // ObjectPickedSignal
    if( highlight )
    {
        //Set the selected DCS
        std::string pathString = boost::any_cast< std::string >
                                 ( mActiveSet->GetPropertyValue( "NodePath" ) );
        osg::NodePath nodePath = osgwTools::stringToNodePath( pathString,
             ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode());
        m_highlightAndSetManipulators( nodePath );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::SyncTransformFromDCS()
{
    if( !mActiveSet.get() )
    {
        return;
    }

    ves::xplorer::Model* model = ves::xplorer::ModelHandler::instance()->GetActiveModel();
    ves::xplorer::ModelCADHandler* mch = model->GetModelCADHandler();
    if( mch->PartExists( mActiveSet->GetUUIDAsString() ) )
    {
        ves::xplorer::scenegraph::CADEntity* cad = mch->GetPart( mActiveSet->GetUUIDAsString() );
        ves::xplorer::scenegraph::DCS* dcs = cad->GetDCS();

        double* trans = dcs->GetVETranslationArray();
        mActiveSet->SetPropertyValue( "Transform_Translation_X", trans[0] );
        mActiveSet->SetPropertyValue( "Transform_Translation_Y", trans[1] );
        mActiveSet->SetPropertyValue( "Transform_Translation_Z", trans[2] );

        double* rot = dcs->GetRotationArray();
        mActiveSet->SetPropertyValue( "Transform_Rotation_X", rot[0] );
        mActiveSet->SetPropertyValue( "Transform_Rotation_Y", rot[1] );
        mActiveSet->SetPropertyValue( "Transform_Rotation_Z", rot[2] );

        double* scale = dcs->GetScaleArray();
        mActiveSet->SetPropertyValue( "Transform_Scale_X", scale[0] );
        mActiveSet->SetPropertyValue( "Transform_Scale_Y", scale[1] );
        mActiveSet->SetPropertyValue( "Transform_Scale_Z", scale[2] );
    }
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

    // Don't repopulate on null selection
    if( nodePath != osg::NodePath() )
    {
        PopulateWithRoot(
            ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot() );
    }

    // Open the tree to this node, but don't attempt to highlight the geometry
    // again
    OpenToAndSelect( nodePath, false );
}

} // namespace conductor
} // namespace ves
