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
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/scenegraph/util/ToggleNodeVisitor.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/xplorer/Debug.h>

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
  *                  x, y, z rotation at the next three indices, and
  *                  x, y, z scale at the last three indices.
  **/
static void TransformCADNode( const std::string& nodeID,
                   const std::vector<double>& transform )
{
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

    if( cadHandler && (meshDetails.size() == 4) )
    {
        const std::string mesh = meshDetails.at(0);
        const std::string lod = meshDetails.at(1);
        const std::string motion = meshDetails.at(2);
        const std::string decimation = meshDetails.at(3);

        if( cadHandler->PartExists( nodeID ) )
        {
            cadHandler->GetPart( nodeID )->
            GetPhysicsRigidBody()->
            CreateRigidBody( lod, motion, mesh, decimation );
            
            vprDEBUG( vesDBG, 1 ) << "|\tChanged Physics Mesh: "
            << cadHandler->GetPart( nodeID )->GetFilename()
            << std::endl << vprDEBUG_FLUSH;
        }
    }
}

//////////////////////////////////////////////////////////////////////////
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


} // namespace cad
} // namespace event
} // namespace xplorer
} // namespace ves
