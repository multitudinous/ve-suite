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

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/scenegraph/util/ToggleNodeVisitor.h>

#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/xplorer/data/CADSubNodePropertySet.h>
#include <ves/xplorer/data/CADPropertySet.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/eventmanager/EventFactory.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Logging.h>

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/model/Model.h>

#include <crunchstore/DataManager.h>
#include <crunchstore/SearchCriterion.h>

/*#if( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 4 ) )
 #include <osg/OcclusionQueryNode>
 #include <ves/xplorer/scenegraph/util/OcclusionQueryVisitor.h>
 #else
 #include <osgOQ/OcclusionQueryNode.h>
 #include <osgOQ/OcclusionQueryVisitor.h>
 #endif*/
#include <osgwQuery/QueryUtils.h>
#include <osgwQuery/QueryComputation.h>

#include <osgwTools/NodePathUtils.h>
#include <osgwTools/Transform.h>

#include <osgwMx/MxUtils.h>

#include <osg/io_utils>
#include <osg/ComputeBoundsVisitor>

#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace cad
{
//////////////////////////////////////////////////////////////////////////
/**
  * Returns the ModelCADHandler for the active model. This function allows the
  * cad slots to operate in a stateless manner.
  */
static ves::xplorer::ModelCADHandler* GetModelCADHandler()
{
    ves::xplorer::Model* activeModel =
        ves::xplorer::ModelHandler::instance()->GetActiveModel();

    if( activeModel )
    {
        return activeModel->GetModelCADHandler();
    }
    else
    {
        return 0;
    }
}
//////////////////////////////////////////////////////////////////////////
/**
  * Applies a transform to a CAD node.
  * @param nodeID The UUID of the CAD node to transform
  * @param transform Reference to the transform to apply. The transform should
  *                  contain x, y, z translation at the first three indices,
  *                  z, x, y rotation at the next three indices, and
  *                  x, y, z scale at the last three indices.
  **/
static void TransformCADNode( const std::string& nodeID,
                              const std::vector<double>& transform )
{
    STATIC_LOG_INFO( "xplorer.CADSlots", "TransformCADNode nodeID: " << nodeID << ", transform: ("
                     << transform[0] << "," << transform[1] << "," << transform[2] << "), ("
                     << transform[3] << "," << transform[4] << "," << transform[5] << "), ("
                     << transform[6] << "," << transform[7] << "," << transform[8] << ")"
                   );
    ModelCADHandler* cadHandler = GetModelCADHandler();

    if( cadHandler )
    {
        ves::xplorer::scenegraph::DCS* dcs = 0;

        if( cadHandler->PartExists( nodeID ) )
        {
            dcs = cadHandler->GetPart( nodeID )->GetDCS();
        }
        else if( cadHandler->AssemblyExists( nodeID ) )
        {
            dcs = cadHandler->GetAssembly( nodeID );
        }

        if( dcs )
        {
            // Entire transform is packed into a single vector. Unpack into
            // separate translation, rotation, and scale pieces.
            std::vector<double>::const_iterator start = transform.begin();
            std::vector<double>::const_iterator stop = transform.begin() + 3;
            std::vector<double> translation( start, stop );
            start += 3;
            stop += 3;
            std::vector<double> rotation( start, stop );
            start += 3;
            stop += 3;
            std::vector<double> scale( start, stop );

            dcs->SetTranslationArray( translation );
            dcs->SetRotationArray( rotation );
            dcs->SetScaleArray( scale );
        }
    }
}
//////////////////////////////////////////////////////////////////////////
/**
  * Sets the opacity on a CAD node.
  * @param nodeID The UUID of the node on which to set opacity
  * @param opacity The opacity on the interval [0...1], where 0 is fully
  *                transparent and 1 is fully opaque
  */
static void SetOpacityOnCADNode( const std::string& nodeID,
                                 double opacity )
{
    ModelCADHandler* cadHandler = GetModelCADHandler();

    if( cadHandler )
    {
        cadHandler->UpdateOpacity( nodeID, opacity );
    }
}
//////////////////////////////////////////////////////////////////////////
/**
  * Turns a CAD node on or off in the scenegraph.
  * @param nodeID The UUID of the node to alter
  * @param visible Whether the CAD should be visible
  */
static void ToggleCADNode( const std::string& nodeID,
                           bool const& visible )
{
    ModelCADHandler* cadHandler = GetModelCADHandler();

    if( cadHandler )
    {
        if( cadHandler->AssemblyExists( nodeID ) )
        {

            ves::xplorer::scenegraph::DCS* tempDCS =
                cadHandler->GetAssembly( nodeID );

            //Toggle all children on/off rather than the assembly
            ves::xplorer::scenegraph::util::ToggleNodeVisitor
            tnv( tempDCS, visible, "" );
        }
        else if( cadHandler->PartExists( nodeID ) )
        {
            cadHandler->GetPart( nodeID )->GetDCS()->
            ToggleDisplay( visible );
            //Now check to make sure parent is toggled on if this was a toggle on

            // ^^^ There was no code following the above comment in CADToggleEH.cxx
        }
    }
}
//////////////////////////////////////////////////////////////////////////
/**
  * Turns CAD node(s) on or off in the scenegraph.
  * @param pattern SQL-style name pattern of node to alter. ( "%" is wildcard)
  * @param visible Whether the CAD should be visible
  */
static void ToggleCADNodeByName( const std::string& pattern,
                           bool const& visible )
{
    std::cout << "ToggleCADNodeByName: " << pattern << ", " << visible << std::endl << std::flush;
    std::vector< std::string > uuids;
    crunchstore::SearchCriterion name( "NameTag", "LIKE", pattern );
    std::vector< crunchstore::SearchCriterion > criteria;
    criteria.push_back( name );
    xplorer::data::DatabaseManager::instance()->GetDataManager()->
            Search( "CADPropertySet", criteria, "uuid", uuids );

    std::cout << "\treturned " << uuids.size() << " results" << std::endl << std::flush;

    // Hide or show everything in the results list.
    for( size_t index = 0; index < uuids.size(); ++index )
    {
        std::cout << "\ttoggling node with id: " << uuids.at(index) << std::endl << std::flush;
        ToggleCADNode( uuids.at( index ), visible );
    }
}
//////////////////////////////////////////////////////////////////////////
/**
 * Turns a sub cad part node on or off in the scenegraph.
 * @param nodeID The UUID of the propertyset
 * @param visible Whether the CAD should be visible
 */
static void ToggleSubCADNode( const std::string& nodeID,
                          bool const& visible )
{
    ves::xplorer::data::CADSubNodePropertySet set;
    set.SetUUID( nodeID );
    set.Load();

    const std::string nodePathStr =
        boost::any_cast<std::string>( set.GetPropertyValue( "NodePath" ) );
    osg::NodePath nodePath = osgwTools::stringToNodePath( nodePathStr,
        ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode() );
    
    //The nodepath always has the root node attached. If it is the last one then clearly
    //the path for the described sub node was not found.
    if( nodePath.back() != ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode() )
    {
        nodePath.back()->setNodeMask( visible );
    }
}
//////////////////////////////////////////////////////////////////////////
static void SetCADPhysicsMesh( const std::string& nodeID,
                               const std::vector<std::string>& meshDetails )
{
    ModelCADHandler* cadHandler = GetModelCADHandler();

    if( !cadHandler->PartExists( nodeID ) )
    {
        return;
    }

    ves::xplorer::scenegraph::CADEntity* part = cadHandler->GetPart( nodeID );

    if( !part->HavePhysics() )
    {
        return;
    }

    if( meshDetails.size() == 4 )
    {
        const std::string motion = meshDetails.at( 0 );
        const std::string lod = meshDetails.at( 1 );
        const std::string mesh = meshDetails.at( 2 );
        const std::string decimation = meshDetails.at( 3 );

        //if( cadHandler->PartExists( nodeID ) )
        {
            part->GetPhysicsRigidBody()->
            CreateRigidBody( lod, motion, mesh, decimation );
            vprDEBUG( vesDBG, 1 ) << "|\tChanged Physics Mesh: "
                                  << part->GetFilename()
                                  << std::endl << vprDEBUG_FLUSH;
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/**
  * Deletes a CAD node from the scenegraph and internal lists.
  * @param parentID UUID of the node's parent
  * @param nodeID UUID of the node
  * @param nodeType Type of node ("Part" or "Assembly")
  */
static void DeleteCADNode( std::string const& parentID, std::string const& nodeID,
                           std::string const& nodeType )
{
    try
    {
        vprDEBUG( vesDBG, 1 ) << "|\t---Deleting node---" << std::endl
                              << vprDEBUG_FLUSH;

        ModelCADHandler* m_cadHandler = GetModelCADHandler();
        ves::xplorer::scenegraph::DCS* parentAssembly =
            m_cadHandler->GetAssembly( parentID );

        //This assumes the part/assembly isn't there already
        if( nodeType == std::string( "Assembly" ) )
        {
            parentAssembly->RemoveChild(
                m_cadHandler->GetAssembly( nodeID ) );
        }
        else if( nodeType == std::string( "Part" ) )
        {
            ves::xplorer::scenegraph::CADEntity* tempPart =
                m_cadHandler->GetPart( nodeID );
            int error = parentAssembly->RemoveChild( tempPart->GetDCS() );
            ///If this node has physics enabled there is an AMT node
            ///between the cadentitity and parent which means this
            ///has to be removed manually.
            if( error == 0 )
            {
                error = parentAssembly->
                        removeChild( tempPart->GetDCS()->getParent( 0 ) );
            }
        }

        //Need to also remove the node from ModelCADHandler node maps
        m_cadHandler->RemoveNode( nodeID, nodeType );
        
        //We need to remove the xml model now
        ves::open::xml::model::ModelPtr xmlModel =
            ves::xplorer::ModelHandler::instance()->GetActiveModel()->GetModelData();
        ves::open::xml::cad::CADAssemblyPtr tempAssembly =
            boost::dynamic_pointer_cast<ves::open::xml::cad::CADAssembly>( xmlModel->GetGeometry() );
        if( tempAssembly->GetID() != parentID )
        {
            tempAssembly =
                boost::dynamic_pointer_cast<ves::open::xml::cad::CADAssembly>( tempAssembly->SearchAllChildren( parentID ) );
        }
        tempAssembly->RemoveChild( nodeID );
    }
    catch( ... )
    {
        std::cout << "Error!!" << std::endl;
        std::cout << "---Invalid node specified to remove!---" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
static void ControlOcclusionQuery( std::string const& nodeID, std::string oqLevel )
{
    ModelCADHandler* cadHandler = GetModelCADHandler();
    if( !cadHandler )
    {
        return;
    }

    if( !cadHandler->PartExists( nodeID ) )
    {
        return;
    }

    osg::ref_ptr< osg::Node > activePart =
        //cadHandler->GetPart( nodeID )->GetNode()->GetNode();
        cadHandler->GetPart( nodeID )->GetDCS();

    osg::ref_ptr< osg::Group > rootPartNode =
        static_cast< osg::Group* >( activePart.get() );

    osgwQuery::RemoveQueries rqs;
    rootPartNode->accept( rqs );

    bool occlude = false;

    if( oqLevel == "Off" )
    {
        occlude = false;
    }
    else if( oqLevel == "Low" )
    {
        occlude = true;
    }
    else if( oqLevel == "Medium" )
    {
        occlude = true;
    }
    else if( oqLevel == "High" )
    {
        occlude = true;
    }

    //if( !root.valid() && occlude )
    if( occlude )
    {
        //osg::ref_ptr< osg::Group > tempGroup = new osg::Group();
        //tempGroup->addChild( tempCADNode.get() );
        /*
         osgOQ::OcclusionQueryNonFlatVisitor oqv;
         //Specify the vertex count threshold for performing
         // occlusion query tests.
         //Settings others use are:
         //Fairly lax culling
         //occlusionThreshold = 5000
         //visibilityThreshold = 250
         //Fairly aggressive culling
         //occlusionThreshold = 2500
         //visibilityThreshold = 500
         // If the child geometry has less than the specified number
         //   of vertices, don't perform occlusion query testing (it's
         //   an occluder). Otherwise, perform occlusion query testing
         //   (it's an occludee).
         oqv.setOccluderThreshold( occlusionThreshold );
         tempGroup->accept( oqv );
         //Setup the number frames to skip
         osgOQ::QueryFrameCountVisitor queryFrameVisitor( 2 );
         tempGroup->accept( queryFrameVisitor );
         // If the occlusion query test indicates that the number of
         //   visible pixels is greater than this value, render the
         //   child geometry. Otherwise, don't render and continue to
         //   test for visibility in future frames.
         osgOQ::VisibilityThresholdVisitor visibilityThresholdVisitor( visibilityThreshold );
         tempGroup->accept( visibilityThresholdVisitor );

         mCadNode = tempGroup.get();*/


        // Any models using occlusion query must render in front-to-back order.
        rootPartNode->getOrCreateStateSet()->setRenderBinDetails( 0, _QUERY_FRONT_TO_BACK_BIN_NAME );

        // Realized the viewer, then root's parent should
        // be the top-level Camera. We want to add queries starting at that node.
        osgwQuery::AddQueries aqs;
        rootPartNode->accept( aqs );
    }

    /*
    unsigned int numOQNs = 0;
    bool oqPresent = false;

    {
        osgOQ::StatisticsVisitor statsVisitor;
        rootPartNode->accept( statsVisitor );
        numOQNs = statsVisitor.getNumOQNs();
        vprDEBUG( vesDBG, 2 ) << "|\t\tThere are currently " << numOQNs
        << "oq nodes." << std::endl
        << vprDEBUG_FLUSH;
    }

    if( numOQNs > 0 )
    {
        oqPresent = true;
    }
    unsigned int occlusionThreshold = 1000;
    unsigned int visibilityThreshold = 100;
    bool occlude = false;

    if( oqSettings == "Off" )
    {
        occlude = false;
    }
    else if( oqSettings == "Low" )
    {
        occlude = true;
        occlusionThreshold = 10000;
        visibilityThreshold = 100;
    }
    else if( oqSettings == "Medium" )
    {
        occlude = true;
        occlusionThreshold = 5000;
        visibilityThreshold = 250;
    }
    else if( oqSettings == "High" )
    {
        occlude = true;
        occlusionThreshold = 2500;
        visibilityThreshold = 500;
    }

    if( !oqPresent && occlude )
    {
        vprDEBUG( vesDBG, 2 ) << "|\t\tCreating new oq nodes " << std::endl
        << vprDEBUG_FLUSH;

        osgOQ::OcclusionQueryNonFlatVisitor oqv;
        //Specify the vertex count threshold for performing
        // occlusion query tests.
        //Settings others use are:
        //Fairly lax culling
        //occlusionThreshold = 5000
        //visibilityThreshold = 250
        //Fairly aggressive culling
        //occlusionThreshold = 2500
        //visibilityThreshold = 500
        // If the child geometry has less than the specified number
        //   of vertices, don't perform occlusion query testing (it's
        //   an occluder). Otherwise, perform occlusion query testing
        //   (it's an occludee).
        oqv.setOccluderThreshold( occlusionThreshold );
        rootPartNode->accept( oqv );
        //Setup the number frames to skip
        osgOQ::QueryFrameCountVisitor queryFrameVisitor( 2 );
        rootPartNode->accept( queryFrameVisitor );
        // If the occlusion query test indicates that the number of
        //   visible pixels is greater than this value, render the
        //   child geometry. Otherwise, don't render and continue to
        //   test for visibility in future frames.
        osgOQ::VisibilityThresholdVisitor visibilityThresholdVisitor( visibilityThreshold );
        rootPartNode->accept( visibilityThresholdVisitor );
    }
    else if( oqPresent && !occlude )
    {
        vprDEBUG( vesDBG, 2 ) << "|\t\tRemoving oq nodes " << std::endl
        << vprDEBUG_FLUSH;

        //remove oq nodes
        osgOQ::RemoveOcclusionQueryVisitor removeOQV;
        rootPartNode->accept( removeOQV );
    }
    else if( oqPresent )
    {
        vprDEBUG( vesDBG, 2 ) << "|\t\tUpdating oq nodes " << std::endl
        << vprDEBUG_FLUSH;
        //remove oq nodes
        osgOQ::RemoveOcclusionQueryVisitor removeOQV;
        rootPartNode->accept( removeOQV );

        //Now create the oq nodes with new settings...
        osgOQ::OcclusionQueryNonFlatVisitor oqv;
        oqv.setOccluderThreshold( occlusionThreshold );
        rootPartNode->accept( oqv );

        osgOQ::QueryFrameCountVisitor queryFrameVisitor( 2 );
        rootPartNode->accept( queryFrameVisitor );

        osgOQ::VisibilityThresholdVisitor visibilityThresholdVisitor( visibilityThreshold );
        rootPartNode->accept( visibilityThresholdVisitor );
    }

    {
        osgOQ::StatisticsVisitor statsVisitor;
        rootPartNode->accept( statsVisitor );
        numOQNs = statsVisitor.getNumOQNs();
        vprDEBUG( vesDBG, 2 ) << "|\t\tThere are now " << numOQNs
        << "oq nodes." << std::endl
        << vprDEBUG_FLUSH;
    }*/
}

//////////////////////////////////////////////////////////////////////////
/**
  * Sets mass of node
  * @param nodeID UUID of the node
  * @param mass Mass
  */
static void SetMassOnCADNode( std::string const& nodeID,
                              double const& mass )
{
    ModelCADHandler* m_cadHandler = GetModelCADHandler();

    if( !m_cadHandler->PartExists( nodeID ) )
    {
        return;
    }

    ves::xplorer::scenegraph::CADEntity* part = m_cadHandler->GetPart( nodeID );

    if( part->HavePhysics() )
    {
        part->GetPhysicsRigidBody()->SetMass( mass );
        std::cout << "Changed Physics Property (Mass): " << mass << " "
                  << part->GetFilename() << std::endl;
    }
}
//////////////////////////////////////////////////////////////////////////
/**
  * Sets friction of node
  * @param nodeID UUID of the node
  * @param friction Friction
  */
static void SetFrictionOnCADNode( std::string const& nodeID,
                                  double const& friction )
{
    ModelCADHandler* m_cadHandler = GetModelCADHandler();

    if( !m_cadHandler->PartExists( nodeID ) )
    {
        return;
    }

    ves::xplorer::scenegraph::CADEntity* part = m_cadHandler->GetPart( nodeID );

    if( part->HavePhysics() )
    {
        part->GetPhysicsRigidBody()->SetFriction( friction );
        std::cout << "Changed Physics Property (Friction): " << friction
                  << " " << part->GetFilename() << std::endl;
    }
}
//////////////////////////////////////////////////////////////////////////
/**
  * Sets restitution of node
  * @param nodeID UUID of the node
  * @param restitution Restitution
  */
static void SetRestitutionOnCADNode( std::string const& nodeID,
                                     double const& restitution )
{
    ModelCADHandler* m_cadHandler = GetModelCADHandler();

    if( !m_cadHandler->PartExists( nodeID ) )
    {
        return;
    }

    ves::xplorer::scenegraph::CADEntity* part = m_cadHandler->GetPart( nodeID );

    if( part->HavePhysics() )
    {
        part->GetPhysicsRigidBody()->SetRestitution( restitution );
        std::cout << "Changed Physics Property (Restitution): "
                  << restitution << " " << part->GetFilename() << std::endl;
    }
}

//////////////////////////////////////////////////////////////////////////
/**
  * Toggles physics on CAD Node
  * @param nodeID UUID of the node
  * @param physics Whether physics is on or not
  */
static void SetPhysicsOnCADNode( std::string const& nodeID,
                                 bool const& physics )
{
    //std::cout << "SetPhysicsOnCADNode: " << physics << std::endl << std::flush;
    ModelCADHandler* m_cadHandler = GetModelCADHandler();

    if( !m_cadHandler->PartExists( nodeID ) )
    {
        return;
    }

    ves::xplorer::scenegraph::CADEntity* part = m_cadHandler->GetPart( nodeID );

    if( physics )
    {
        part->InitPhysics();

        // Once physics is initialized, we need to make sure the current physical
        // properties and mesh settings get applied.
        ves::xplorer::data::CADPropertySet set;
        set.SetUUID( nodeID );
        set.Load();

        SetMassOnCADNode( nodeID, boost::any_cast<double>( set.GetPropertyValue( "Physics_Mass" ) ) );
        SetFrictionOnCADNode( nodeID, boost::any_cast<double>( set.GetPropertyValue( "Physics_Friction" ) ) );
        SetRestitutionOnCADNode( nodeID, boost::any_cast<double>( set.GetPropertyValue( "Physics_Restitution" ) ) );

        std::vector<std::string> meshDetails;
        meshDetails.push_back( boost::any_cast<std::string>( set.GetPropertyValue( "Physics_MotionType" ) ) );
        meshDetails.push_back( boost::any_cast<std::string>( set.GetPropertyValue( "Physics_LODType" ) ) );
        meshDetails.push_back( boost::any_cast<std::string>( set.GetPropertyValue( "Physics_MeshType" ) ) );
        meshDetails.push_back( boost::any_cast<std::string>( set.GetPropertyValue( "Physics_MeshDecimation" ) ) );
        SetCADPhysicsMesh( nodeID, meshDetails );
    }
    else
    {
        if( part->HavePhysics() )
        {
            std::cout << "Cleaning physics rigid body." << std::endl;
            //part->GetPhysicsRigidBody()->CleanRigidBody();
            part->DisablePhysics();
        }
    }

    // Scenegraph will have changed after this operation; announce this
    reinterpret_cast< ves::util::VoidSignal_type* >
    ( xplorer::eventmanager::EventFactory::instance()->GetSignal( "ScenegraphChanged" ) )
    ->signal();
}
////////////////////////////////////////////////////////////////////////////////
///Sets mass of node
///@param nodeID UUID of the node
///@param flag True/false to set whether to be transparent or not
static void SetVizTransparencyFlag( std::string const& nodeID, bool const& flag )
{
    ModelCADHandler* m_cadHandler = GetModelCADHandler();
    
    if( !m_cadHandler->PartExists( nodeID ) )
    {
        return;
    }
    
    ves::xplorer::scenegraph::CADEntity* part = m_cadHandler->GetPart( nodeID );
    part->SetTransparencyFlag( flag );
}
////////////////////////////////////////////////////////////////////////////////
///Sets mass of node
///@param nodePath Node path of the selected node
static void NavigateToNode( osg::NodePath const& nodePath )
{    
    //To make this work we must:
    //1. get current state of world dcs
    //2. set dcs back to zero.
    //3. figure out where to set the center of the world
    //4. set the new rotation
    //5. get the vector of the this location
    //6. hand of to nav animation engine
    //7. reset the world dcs back to original state
    
    //Set the center point to the new node position
    //vx::DeviceHandler::instance()->SetSelectedDCS( selectedDCS.get() );

    osg::ref_ptr< osg::Node > selectedNode = nodePath.back();
    osg::BoundingSphere bs = selectedNode->getBound();
    
    //osg::ComputeBoundsVisitor cbbv( osg::NodeVisitor::TRAVERSE_ALL_CHILDREN );
    //selectedNode->accept( cbbv );
    //osg::BoundingBox bb = cbbv.getBoundingBox();

    //We must pop the last matrix so that local transform information
    //is not applied twice to the bounding sphere in the osgwTools::transform call.
    osg::NodePath tempPath = nodePath;
    if( tempPath.size() > 1 )
    {
        tempPath.pop_back();
    }
    osg::Matrixd bsMat = osg::computeLocalToWorld( tempPath );
    osg::BoundingSphere sbs = osgwTools::transform( bsMat, bs );

    /*std::cout
    << "|\tBounding Box Info" << std::endl
    << "|\tCenter " << bb.center() << std::endl
    << "|\tRadius " << bb.radius() << std::endl
    << "|\tMin " << bb._min << std::endl
    << "|\tMax " << bb._max << std::endl;
    std::cout
    << "|\tBounding Sphere Info" << std::endl
    << "|\tCenter " << sbs.center() << std::endl
    << "|\tRadius " << sbs.radius() << std::endl;*/

    
    //Calculate the offset distance
    double fovz = ves::xplorer::scenegraph::SceneManager::instance()->GetCurrentGLTransformInfo()->GetFOVZ();
    //We need a scaled radius here
    double mxdistance = osgwMx::computeInitialDistanceFromFOVY( sbs, osg::RadiansToDegrees( fovz ) );
    
    ///Get the location of the selected model in local coordinates
    ///This value is always the same no matter where we are
    gmtl::Point3d osgTransformedPosition;
    gmtl::Point3d osgTransformedPosition2;
    gmtl::Point3d osgOrigPosition;
    osgTransformedPosition[ 0 ] = sbs.center().x();
    osgTransformedPosition[ 2 ] = sbs.center().y() + mxdistance;
    osgTransformedPosition[ 1 ] = sbs.center().z();
    osgOrigPosition[ 0 ] = sbs.center().x();
    osgOrigPosition[ 1 ] = sbs.center().y();
    osgOrigPosition[ 2 ] = sbs.center().z();
    osgTransformedPosition2 = osgTransformedPosition * -1.0;

    //This rotates the view back to oriented at 0,0,0
    //double tempRotRad2 = osg::DegreesToRadians( -90.0 );
    //gmtl::AxisAngled axisAngle( tempRotRad2, 1, 0, 0 );
    //gmtl::Quatd quatAxisAngle = gmtl::make< gmtl::Quatd >( axisAngle );

    osgwMx::MxCore& core =
        ves::xplorer::scenegraph::SceneManager::instance()->GetMxCoreViewMatrix();
    osg::Matrixd currentView = core.getInverseMatrix();
    
    //convert osg quat to gmtl quat
    osg::Quat tempWorldQuat = currentView.getRotate();
    gmtl::Quatd quatAxisAngle( tempWorldQuat[0], tempWorldQuat[1],
                        tempWorldQuat[2], tempWorldQuat[3] );
    
    ///Hand our created end points off to the animation engine
    ves::xplorer::NavigationAnimationEngine::instance()->SetAnimationEndPoints(
        osgTransformedPosition2, quatAxisAngle, true, NULL );
}
////////////////////////////////////////////////////////////////////////////////
///Enable two sided lighting on the selected CAD file
///\param enableTwoSidedLighting Turn two sided lighting off/on
void EnableTwoSidedLighting( const std::string& nodeUUID, const bool& enableTwoSidedLighting )
{
    ModelCADHandler* m_cadHandler = GetModelCADHandler();
    
    if( !m_cadHandler->PartExists( nodeUUID ) )
    {
        return;
    }

    ves::xplorer::scenegraph::CADEntity* part = m_cadHandler->GetPart( nodeUUID );
    part->SetTwoSidedLighting( enableTwoSidedLighting );
}
////////////////////////////////////////////////////////////////////////////////
} // namespace cad
} // namespace event
} // namespace xplorer
} // namespace ves
