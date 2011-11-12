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

#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/scenegraph/util/ToggleNodeVisitor.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/data/CADPropertySet.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Logging.h>

/*#if( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 4 ) )
 #include <osg/OcclusionQueryNode>
 #include <ves/xplorer/scenegraph/util/OcclusionQueryVisitor.h>
 #else
 #include <osgOQ/OcclusionQueryNode.h>
 #include <osgOQ/OcclusionQueryVisitor.h>
 #endif*/
#include <osgwQuery/QueryUtils.h>
#include <osgwQuery/QueryComputation.h>

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
    STATIC_LOG_INFO("xplorer.CADSlots","TransformCADNode nodeID: " << nodeID << ", transform: ("
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
            std::vector<double> translation( start, stop  );
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
                    bool visible )
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
        else if ( cadHandler->PartExists( nodeID ) )
        {
            cadHandler->GetPart( nodeID )->GetDCS()->
                ToggleDisplay( visible );
            //Now check to make sure parent is toggled on if this was a toggle on

            // ^^^ There was no code following the above comment in CADToggleEH.cxx
        }
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
        const std::string motion = meshDetails.at(0);
        const std::string lod = meshDetails.at(1);
        const std::string mesh = meshDetails.at(2);
        const std::string decimation = meshDetails.at(3);

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
        ves::xplorer::scenegraph::DCS* parentAssembly = 0;
        parentAssembly = m_cadHandler->GetAssembly( parentID );

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
        else if( nodeType == std::string( "Clone" ) )
        {
            parentAssembly->RemoveChild( m_cadHandler->GetClone( nodeID )->GetClonedGraph() );
        }
        //Need to also remove the node from ModelCADHandler node maps
        m_cadHandler->RemoveNode( nodeID, nodeType );
    }
    catch ( ... )
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
        set.LoadFromDatabase();

        SetMassOnCADNode( nodeID, boost::any_cast<double>(set.GetPropertyValue("Physics_Mass")) );
        SetFrictionOnCADNode( nodeID, boost::any_cast<double>(set.GetPropertyValue("Physics_Friction")) );
        SetRestitutionOnCADNode( nodeID, boost::any_cast<double>(set.GetPropertyValue("Physics_Restitution")) );

        std::vector<std::string> meshDetails;
        meshDetails.push_back( boost::any_cast<std::string>( set.GetPropertyAttribute( "Physics_MotionType", "enumCurrentString" ) ) );
        meshDetails.push_back( boost::any_cast<std::string>( set.GetPropertyAttribute( "Physics_LODType", "enumCurrentString" ) ) );
        meshDetails.push_back( boost::any_cast<std::string>( set.GetPropertyAttribute( "Physics_MeshType", "enumCurrentString" ) ) );
        meshDetails.push_back( boost::any_cast<std::string>( set.GetPropertyAttribute( "Physics_MeshDecimation", "enumCurrentString" ) ) );
        SetCADPhysicsMesh( nodeID, meshDetails );
    }
    else
    {
        if( part->HavePhysics() )
        {
            std::cout << "Cleaning physics rigid body." << std::endl;
            part->GetPhysicsRigidBody()->CleanRigidBody();
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
} // namespace cad
} // namespace event
} // namespace xplorer
} // namespace ves
