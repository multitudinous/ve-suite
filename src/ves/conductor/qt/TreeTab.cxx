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
#include <ves/conductor/qt/TreeTab.h>

#include <osgQtTree/osgQtTree.h>
#include <osgQtTree/treemodel.h>
#include <osgQtTree/osgTreeItem.h>

#include <QtGui/QScrollBar>
#include <QtCore/QUuid>

#include <ves/conductor/qt/ui_TreeTab.h>

#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/data/CADSubNodePropertySet.h>
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/Model.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <ves/xplorer/eventmanager/EventFactory.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>

#include <ves/open/xml/cad/CADNode.h>

#include <osgwTools/NodePathUtils.h>
#include <osgwTools/Orientation.h>

#include <crunchstore/SearchCriterion.h>

#include <boost/algorithm/string/replace.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>

#include <gmtl/Coord.h>
#include <gmtl/Generate.h>
#include <gmtl/EulerAngle.h>
#include <gmtl/AxisAngle.h>

#include <iostream>

Q_DECLARE_METATYPE( osg::NodePath )
Q_DECLARE_METATYPE( std::string )

namespace ves
{
namespace conductor
{

TreeTab::TreeTab( QWidget* parent ) :
    QWidget( parent ),
    ui( new Ui::TreeTab ),
    m_logger( Poco::Logger::get( "conductor.TreeTab" ) ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    ui->setupUi( this );

    ////////////// Making a Tree /////////////////////
    // Create a tree model and set the column headers on it
    mModel = new TreeModel;
    QList<QVariant> rootData;
    rootData << "Node Name";
    mModel->CreateRootItem( rootData );

    // Connect the tree model up to a tree view.
    ui->mTreeView->setModel( mModel );
    ui->mTreeView->header()->setResizeMode( QHeaderView::ResizeToContents );

    qRegisterMetaType<osg::NodePath>();
    qRegisterMetaType<std::string>();
    QObject::connect( this, SIGNAL( ObjectPicked( osg::NodePath ) ),
                      this, SLOT( QueuedOnObjectPicked( osg::NodePath ) ),
                      Qt::QueuedConnection );

    QObject::connect( this, SIGNAL( NodeAddedQSignal( std::string ) ),
                      this, SLOT( QueuedNodeAdded( std::string ) ),
                      Qt::QueuedConnection );

    // Connect to ObjectPickedSignal so we can update the scenegraph tree view when
    // an object is picked
    CONNECTSIGNALS_1( "%ObjectPickedSignal",
                      void( osg::NodePath& ),
                      &TreeTab::OnObjectPicked,
                      mConnections, any_SignalType, normal_Priority );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_highlightAndSetManipulators ),
        "TreeTab.HighlightAndSetManipulators" );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_highlightNode ),
        "TreeTab.HighlightNode" );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_CADNodeSelected ),
        "TreeTab.CADNodeSelected" );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_navToNode ),
        "TreeTab.NavigateToNode" );
    
    CONNECTSIGNALS_1( "%NodeAdded",
                      void ( std::string const& ),
                      &TreeTab::OnNodeAdded,
                      mConnections, any_SignalType, normal_Priority );

    CONNECTSIGNAL_1( "AddVTKDataSetEventHandler.DatafileLoaded",
                     void ( std::string const& ),
                     &TreeTab::OnNodeAdded,
                     mConnections, normal_Priority );

    CONNECTSIGNAL_0( "ScenegraphChanged",
                     void( ),
                     &TreeTab::ScenegraphChanged,
                     mConnections, normal_Priority );

}
////////////////////////////////////////////////////////////////////////////////
TreeTab::~TreeTab()
{
    delete mModel;
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::changeEvent( QEvent* e )
{
    QWidget::changeEvent( e );
    switch( e->type() )
    {
    case QEvent::LanguageChange:
        ui->retranslateUi( this );
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
    osgQtTree::PopulateTreeControlWithNodeVisitor populator( ( ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode() ), mModel );
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
    LOG_DEBUG( "Select" );
    //Unselect the previously-selected DCS
    if( highlight )
    {
        ves::xplorer::DeviceHandler::instance()->UnselectObjects();
    }

    // Write out the previously-selected CADPropertySet so that changes are not
    // lost
    if( mActiveSet.get() != 0 )
    {
        mActiveSet->Save();
    }

    // Get the node associated with this QModelIndex
    osg::Node* node = 0;
    std::string nodePathStr;
    if( index != QModelIndex() )
    {
        osgQtTree::osgTreeItem* item = static_cast< osgQtTree::osgTreeItem* >( index.internalPointer() );
        node = item->GetNode();
        nodePathStr = item->GetNodePath();
    }

    // See if this node has a VE_XML_ID
    std::string type;
    bool found = false;
    if( node )
    {
        //std::cout << "Node description list:" << std::endl << std::flush;
        osg::Node::DescriptionList descList = node->getDescriptions();
        for( size_t i = 0; i < descList.size(); ++i )
        {
            //std::cout << descList.at( i ) << std::endl << std::flush;
            if( descList.at( i ) == "VE_XML_ID" )
            {
                type = "CAD";
                found = true;
                break;
            }
            else if( descList.at( i ) == "VE_DATA_NODE" )
            {
                type = "DATA";
                found = true;
                break;
            }
        }
    }

    //If not node and not found
    if( !found )
    {
        LOG_DEBUG( "No matching DCS" );

        ///Because we want to be able to select nodes for constraints and
        ///other per part operations we need to highlight the selected node
        ///even if it is not a top level node
        if( node )
        {
            ves::xplorer::scenegraph::FindParentsVisitor
            parentVisitor( node, ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode() );
            LOG_DEBUG( "Trying to select " << node->getName() );
            osg::NodePath nodePath = parentVisitor.GetParentNodePath();

            const std::string uuid = CreateSubNodePropertySet( node, nodePath );
            mActiveSet =
                propertystore::PropertySetPtr( new ves::xplorer::data::CADSubNodePropertySet() );
            mActiveSet->SetUUID( uuid );
            mActiveSet->Load();
            mActiveSet->EnableLiveProperties( true );
            ui->cadPropertyBrowser->ParsePropertySet( mActiveSet, false );
            m_highlightNode.signal( nodePath );
        }
        else
        {
            // Clear out the PropertyBrowser widget
            propertystore::PropertySetPtr nullPtr;
            ui->cadPropertyBrowser->ParsePropertySet( nullPtr, false );
            mActiveSet = nullPtr;
        }

        return;
    }
    LOG_DEBUG( "Node is of type " << type );

    // Create a CADPropertySet and load it in the browser
    if( type == "CAD" )
    {
        ves::xplorer::scenegraph::DCS* newSelectedDCS =
            static_cast< ves::xplorer::scenegraph::DCS* >( node );
        mActiveSet = propertystore::PropertySetPtr( new ves::xplorer::data::CADPropertySet() );
        mActiveSet->SetUUID( newSelectedDCS->GetCADPart()->GetID() );
    }
    else if( type == "DATA" )
    {
        mActiveSet = propertystore::PropertySetPtr( new ves::xplorer::data::DatasetPropertySet() );
        mActiveSet->SetUUID( node->getDescriptions().at( 1 ) );
    }

    mActiveSet->Load();
    ui->cadPropertyBrowser->ParsePropertySet( mActiveSet, false );

    osg::PositionAttitudeTransform* trans =
        dynamic_cast< osg::PositionAttitudeTransform* >( node );

    SyncTransformFromDCS( trans );
    // Turn on live updates
    mActiveSet->EnableLiveProperties( true );

    // highlight will be true if this method was called from the tree widget in
    // any form, and false if it was called as a side-effect of
    // ObjectPickedSignal
    if( highlight && type == "CAD" )
    {
        //Set the selected DCS
        osg::NodePath nodePath = osgwTools::stringToNodePath( nodePathStr,
            ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode() );
        m_highlightAndSetManipulators.signal( nodePath );
    }

    if( type == "CAD" )
    {
        LOG_DEBUG( "Firing CADNodeSelected signal" );
        m_CADNodeSelected.signal( nodePathStr );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::SyncTransformFromDCS( osg::PositionAttitudeTransform* dcs )
{
    if( !mActiveSet.get() )
    {
        return;
    }

    {
        osg::Vec3d trans = dcs->getPosition();
        mActiveSet->SetPropertyValue( "Transform_Translation_X", trans[0] );
        mActiveSet->SetPropertyValue( "Transform_Translation_Y", trans[1] );
        mActiveSet->SetPropertyValue( "Transform_Translation_Z", trans[2] );

        // Rotation angles are in order z, x, y
        {
            osg::Quat quat = dcs->getAttitude();
            double rot[ 3 ];
            osg::ref_ptr< osgwTools::Orientation > tempQuat = new osgwTools::Orientation();
            tempQuat->getYPR( quat, rot[0], rot[1], rot[2] );

            mActiveSet->SetPropertyValue( "Transform_Rotation_X", rot[1] );
            mActiveSet->SetPropertyValue( "Transform_Rotation_Y", rot[2] );
            mActiveSet->SetPropertyValue( "Transform_Rotation_Z", rot[0] );

            /*gmtl::Quatd tempQuat( quat[0], quat[1], quat[2], quat[3] );
            gmtl::Matrix44d _vjMatrix = gmtl::makeRot< gmtl::Matrix44d >( tempQuat );
            gmtl::EulerAngleZXYd tempZXY = gmtl::makeRot< gmtl::EulerAngleZXYd >( _vjMatrix );
            
            rot[0] = gmtl::Math::rad2Deg( tempZXY[0] );
            rot[1] = gmtl::Math::rad2Deg( tempZXY[1] );
            rot[2] = gmtl::Math::rad2Deg( tempZXY[2] );
            
            mActiveSet->SetPropertyValue( "Transform_Rotation_X", rot[1] );
            mActiveSet->SetPropertyValue( "Transform_Rotation_Y", rot[2] );
            mActiveSet->SetPropertyValue( "Transform_Rotation_Z", rot[0] );*/
        }

        osg::Vec3d scale = dcs->getScale();
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
        mActiveSet->Load();
        ui->cadPropertyBrowser->RefreshAllValues();
    }
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_OKButton_clicked()
{
    if( mActiveSet )
    {
        mActiveSet->Save();
    }
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_m_navToButton_clicked()
{
    QModelIndex modelIndex = ui->mTreeView->currentIndex();
    if( modelIndex.isValid() )
    {
        osgQtTree::osgTreeItem* item = static_cast< osgQtTree::osgTreeItem* >( modelIndex.internalPointer() );
        osg::NodePath nodePath =
            osgwTools::stringToNodePath( item->GetNodePath(),
            ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode() );
        m_navToNode.signal( nodePath );
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
    // Open the tree to this node, but don't attempt to highlight the geometry
    // again
    OpenToAndSelect( nodePath, false );
}
////////////////////////////////////////////////////////////////////////////////
std::string TreeTab::GetSelectedNodeID()
{
    if( mActiveSet )
    {
        return mActiveSet->GetUUIDAsString();
    }
    else
    {
        return std::string();
    }
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::OnNodeAdded( std::string const& filename )
{
    NodeAddedQSignal( filename );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::ScenegraphChanged()
{
    NodeAddedQSignal( std::string() );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::QueuedNodeAdded( std::string const& )
{
    RefreshTree();
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::RefreshTree()
{
    // Read the scenegraph and rebuild tree.
    PopulateWithRoot(
        &( ves::xplorer::scenegraph::SceneManager::instance()->GetGraphicalPluginManager() ) );
    ui->m_expandAllButton->setText( "Expand All" );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_m_searchBox_textEdited( const QString& pattern )
{
    ui->mTreeView->expandAll();
    ui->mTreeView->keyboardSearch( pattern );
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_m_deleteButton_clicked()
{
    ves::xplorer::Model* model = ves::xplorer::ModelHandler::instance()->GetActiveModel();
    if( !model )
    {
        return;
    }
    ves::xplorer::ModelCADHandler* mch = model->GetModelCADHandler();
    if( !mch )
    {
        return;
    }

    QModelIndex modelIndex = ui->mTreeView->currentIndex();
    osg::Node* node = 
        static_cast< osgQtTree::osgTreeItem* >( modelIndex.internalPointer() )->GetNode();
    if( !node )
    {
        std::cout << "Selected node does not have a parent node." << std::endl;
        return;
    }

    bool found = false;
    osg::Node::DescriptionList descList = node->getDescriptions();
    for( size_t i = 0; i < descList.size(); ++i )
    {
        // See if this node has a VE_XML_ID
        if( descList.at( i ) == "VE_XML_ID" )
        {
            found = true;
            DeleteCADNode( node );
            break;
        }
        else if( descList.at( i ) == "VE_DATA_NODE" )
        {
            found = true;
            DeleteDataNode( node );
            break;
        }
    }

    if( !found )
    {
        std::cout << "Trying to delete node but couldn't find parent" << std::endl << std::flush;
        return;
    }

    RefreshTree();
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::on_m_expandAllButton_clicked()
{
    if( ui->m_expandAllButton->text() == QString( "Expand All" ) )
    {
        ui->mTreeView->expandAll();
        ui->m_expandAllButton->setText( "Collapse All" );
    }
    else
    {
        ui->mTreeView->collapseAll();
        ui->m_expandAllButton->setText( "Expand All" );
    }
}
////////////////////////////////////////////////////////////////////////////////
std::string TreeTab::CreateSubNodePropertySet( osg::Node* node, osg::NodePath& path )
{
    std::string pathString = osgwTools::nodePathToString( path );

    //First check to see if the current node path is in the db
    crunchstore::SearchCriterion critNodePath( "NodePath", "=", pathString );
    std::vector< crunchstore::SearchCriterion > criteria;
    criteria.push_back( critNodePath );
    std::vector< std::string > results;
    ves::xplorer::data::DatabaseManager::instance()->GetDataManager()->
        Search( "CADSubNodePropertySet", criteria, "uuid", results );

    if( !results.empty() )
    {
        return( results.back() );
    }
    
    ves::xplorer::data::CADSubNodePropertySet newSet;
    std::string uuid = boost::lexical_cast< std::string >( boost::uuids::random_generator()() );
    newSet.SetUUID( uuid );
    newSet.SetPropertyValue( "NameTag", node->getName() );
    /*
        //if( newPart->HasPhysics() )
        {
            newSet.SetPropertyValue( "Physics",
                                    newPart->HasPhysics() );
            newSet.SetPropertyValue( "Physics_Mass",
                                    newPart->GetMass() );
            newSet.SetPropertyValue( "Physics_Friction",
                                    newPart->GetFriction() );
            newSet.SetPropertyValue( "Physics_Restitution",
                                    newPart->GetRestitution() );
            newSet.SetPropertyValue( "Physics_MotionType",
                                    newPart->GetPhysicsMotionType() );
            newSet.SetPropertyValue( "Physics_LODType",
                                    newPart->GetPhysicsLODType() );
            newSet.SetPropertyValue( "Physics_MeshType",
                                    newPart->GetPhysicsMeshType() );
            newSet.SetPropertyValue( "Physics_MeshDecimation",
                                    newPart->GetPhysicsDecimationValue() );
        }
    */

    // Calculate and store the NodePath
    newSet.SetPropertyValue( "NodePath", pathString );
    //newSet.SetPropertyValue( "Visibile", newPart->GetVisibility() );
    newSet.Save();

    return uuid;
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::DeleteCADNode( osg::Node* )
{
    QModelIndex modelIndex = ui->mTreeView->currentIndex();
    QModelIndex parentIndex = mModel->parent( modelIndex );
    //We have to get the parent item so that we can get the parent uuid
    osgQtTree::osgTreeItem* parentItem = 
        static_cast< osgQtTree::osgTreeItem* >( parentIndex.internalPointer() );
    osg::Node* parentNode = parentItem->GetNode();
    
    ves::xplorer::scenegraph::DCS* newSelectedDCS = 
        static_cast< ves::xplorer::scenegraph::DCS* >( parentNode );
    
    const std::string parentID = newSelectedDCS->GetCADPart()->GetID();
    //This the actual node we selected
    const std::string nodeID = mActiveSet->GetUUIDAsString();
    std::string type;
    ves::xplorer::ModelCADHandler* mch = 
        ves::xplorer::ModelHandler::instance()->GetActiveModel()->GetModelCADHandler();
    if( mch->PartExists( nodeID ) )
    {
        //std::cout << "Part exists...removing..." << std::endl << std::flush;
        type = "Part";
    }
    else if( mch->AssemblyExists( nodeID ) )
    {
        //std::cout << "Assembly exists...removing..." << std::endl << std::flush;
        type = "Assembly";
    }
    
    using namespace ves::xplorer;
    reinterpret_cast< ves::util::ThreeStringSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "DeleteCADNode" ) )
    ->signal( parentID, nodeID, type );
    mActiveSet->Remove();
    mActiveSet = propertystore::PropertySetPtr();
}
////////////////////////////////////////////////////////////////////////////////
void TreeTab::DeleteDataNode( osg::Node* )
{ 
    const std::string& datasetName =
        boost::any_cast< std::string >( mActiveSet->GetPropertyValue( "Filename" ) );
    using namespace ves::xplorer;
    reinterpret_cast< ves::util::StringSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "DeleteDataSet" ) )
    ->signal( datasetName );
    mActiveSet->Remove();
    mActiveSet = propertystore::PropertySetPtr();
}
////////////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
