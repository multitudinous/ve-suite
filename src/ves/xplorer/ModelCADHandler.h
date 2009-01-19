/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef VE_XPLORER_MODEL_CAD_HANDLER_H
#define VE_XPLORER_MODEL_CAD_HANDLER_H

#include <ves/xplorer/ModelCADHandlerPtr.h>
#include <ves/xplorer/GlobalBase.h>

#include <ves/open/xml/cad/CADNodePtr.h>
#include <ves/open/xml/cad/CADAttributePtr.h>

#include <osg/StateSet>

#include <map>
#include <string>
#include <vector>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class CADEntity;
class CADEntityHelper;
class Clone;
}
}
}

namespace osg
{
class ClipPlane;
}

namespace ves
{
namespace xplorer
{
/*!\file ModelCADHandler.h
Mananger for the CAD associated with a cfdModel
*/
/*!\class ves::xplorer::ModelCADHandler
*
*/
class VE_XPLORER_EXPORTS ModelCADHandler : public GlobalBase
{
public:
    ///Constructor
    ///\param rootCADNode The top-level CAD
    ModelCADHandler( ves::xplorer::scenegraph::DCS* rootCADNode );
    ///Copy Construtor
    ///\param rhs The ModelCADHandler we are copying.
    ModelCADHandler( const ModelCADHandler& rhs );

    ///Destructor
    virtual ~ModelCADHandler();

    ///Set the UUID of the root CADNode
    ///\param rootNodeId The uuid of the root CADNode in Conductor
    void SetRootCADNodeID( std::string rootNodeId );

    ///\param CAD goes transparent when dataset vis is active
    void MakeCADRootTransparent();

    ///\param CAD return to default state on clear all
    void MakeCADRootOpaque();

    ///Add a new attribute to a node
    ///\param nodeID The ID of the node to add Attribute to.
    ///\param nodeType The node type.
    ///\param The CADAttribute to add to the node.
    void AddAttributeToNode( std::string nodeID,
                             ves::open::xml::cad::CADAttributePtr newAttribute );
    ///Add a new attribute to a node
    ///\param nodeID The ID of the node to add Attribute to.
    ///\param nodeType The node type.
    ///\param neAttribute The name of the CADAttribute to remove from the node.
    void RemoveAttributeFromNode( std::string nodeID,
                                  std::string nodeType,
                                  std::string newAttribute );
    ///Add a new attribute to a node
    ///\param nodeID The ID of the node to add Attribute to.
    ///\param nodeType The node type.
    ///\param attributeName The name of the CADAttribute to activate on the CADNode.
    void SetActiveAttributeOnNode( std::string nodeID,
                                   std::string nodeType,
                                   std::string attributeName );
    ///Create a new assembly
    void CreateAssembly( std::string assemblyID );

    ///Create a new clone
    void CreateClone( std::string cloneID,
                      std::string originalID,
                      std::string orignalType );

    ///Create a new part
    void CreatePart( std::string fileName,
                     std::string partID,
                     std::string parentID );

    ///Clear out the associated information for a node
    ///\param nodeID The node id
    ///\param nodeTyp The type of node
    void RemoveNode( std::string nodeID,
                     std::string nodeType );

    ///Get a specified CADAttribute for a specified CADNode
    ///\param nodeID The CADNode
    ///\param attributeName The name of the CADAttribute to find.
    ///\param component The name of the CADMaterial component to update.
    ///\param face The face to apply the update to.
    ///\param values The new values.
    void UpdateMaterialComponent( std::string nodeID,
                                  std::string attributeName, std::string component,
                                  std::string face,
                                  std::vector<double> values );

    ///Get a specified CADAttribute for a specified CADNode
    ///\param nodeID The CADNode
    ///\param attributeName The name of the CADAttribute to find.
    ///\param type The type of mode to update.
    ///\param mode The new mode.
    void UpdateMaterialMode( std::string nodeID,
                             std::string attributeName,
                             std::string type,
                             std::string mode );

    ///Update the opacity for a specified CADNode
    ///\param nodeID The CADNode
    ///\param opacity The value of the opacity.
    ///\param storeState Tell the opacity visitor wether or not to 
    ///store the original color and material state
    void UpdateOpacity( std::string nodeID, float opacity, bool storeState );

    ///Get a specific part.
    ///\param partID The ID of the part to search form
    ves::xplorer::scenegraph::CADEntity* GetPart( std::string partID );

    ///Get a specific assembly.
    ///\param assemblyID The ID of the assembly to search form
    ves::xplorer::scenegraph::DCS* GetAssembly( std::string assemblyID );

    ///Get a specific assembly.
    ///\param assemblyID The ID of the assembly to search form
    ves::xplorer::scenegraph::Clone* GetClone( std::string cloneID );

    ///\param cloneID The part ID to search for.
    bool CloneExists( std::string clone );

    ///\param partID The part ID to search for.
    bool PartExists( std::string partID );

    ///\param assemblyID The assembly ID to search for.
    bool AssemblyExists( std::string assemblyID );
    ///The current graph
    std::string GetRootCADNodeID();

    ///Set the clip plane equation
    ///\param a X coefficient
    ///\param b Y coefficient
    ///\param c Z coefficient
    ///\param d
    void SetClipPlane( double a, double b,
                       double c, double d );
    ///Turn the clip plane off or on
    ///\param onOff The state of the ClipPlane
    void ToggleClipPlane( bool onOff );
    ///Equal operator
    ///\param rhs The ModelCADHandler we are setting this one equal to.
    ModelCADHandler& operator=( const ModelCADHandler& rhs );

    ///Not used
    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand()
    {
        ;
    }

protected:
    ///The clipping plane for geometry
    osg::ref_ptr<osg::ClipPlane> m_clipPlane;
    ///A list of the current parts.
    std::map< std::string, ves::xplorer::scenegraph::CADEntity* > m_partList;
    ///A list of the current assemblies.
    std::map< std::string, osg::ref_ptr< ves::xplorer::scenegraph::DCS > > m_assemblyList;
    ///A list of clones.
    std::map< std::string, ves::xplorer::scenegraph::Clone* > m_cloneList;
    ///ID for root CAD node id
    std::string m_rootCADNodeID;
    ///Attribute list mapping for all CAD
    std::map<std::string, osg::ref_ptr< osg::StateSet> > m_globalAttributeList;
#ifdef _OSG
    ///The map of node attributes.
    std::map < std::string, std::vector <
    std::pair< std::string, osg::ref_ptr< osg::StateSet > > >
    > m_nodeAttributes;
#endif
};
}
}
#endif // MODEL_CAD_HANDLER_H
