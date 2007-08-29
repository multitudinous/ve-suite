/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef MODEL_CAD_HANDLER_H
#define MODEL_CAD_HANDLER_H
/*!\file ModelCADHandler.h
Mananger for the CAD associated with a cfdModel
*/
/*!\class VE_Xplorer::ModelCADHandler
* 
*/
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

#include "VE_Installer/include/VEConfig.h"

#include <map>
#include <string>
#include <vector>

namespace VE_SceneGraph
{
    class DCS;
    class CADEntity;
    class CADEntityHelper;
    class Clone;
}
namespace VE_XML
{
namespace VE_CAD
{
    class CADNode;
    class CADAttribute;
}
}
#include <osg/StateSet>

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS ModelCADHandler : public cfdGlobalBase
{
public:
    ///Constructor
    ///\param rootCADNode The top-level CAD
    ModelCADHandler(VE_SceneGraph::DCS* rootCADNode);
    ///Copy Construtor
    ///\param rhs The ModelCADHandler we are copying.
    ModelCADHandler(const ModelCADHandler& rhs);
    
    ///Destructor
    virtual ~ModelCADHandler();

    ///Set the UUID of the root CADNode 
    ///\param rootNodeId The uuid of the root CADNode in Conductor
    void SetRootCADNodeID(std::string rootNodeId);

    ///\param CAD goes transparent when dataset vis is active
    void MakeCADRootTransparent();

    ///\param CAD return to default state on clear all
    void MakeCADRootOpaque();

    ///Add a new attribute to a node
    ///\param nodeID The ID of the node to add Attribute to.
    ///\param nodeType The node type.
    ///\param The CADAttribute to add to the node.
    void AddAttributeToNode(std::string nodeID,
                                      VE_XML::VE_CAD::CADAttribute* newAttribute);
    ///Add a new attribute to a node
    ///\param nodeID The ID of the node to add Attribute to.
    ///\param nodeType The node type.
    ///\param neAttribute The name of the CADAttribute to remove from the node.
    void RemoveAttributeFromNode(std::string nodeID,
                                             std::string nodeType,
                                             std::string newAttribute);
    ///Add a new attribute to a node
    ///\param nodeID The ID of the node to add Attribute to.
    ///\param nodeType The node type.
    ///\param attributeName The name of the CADAttribute to activate on the CADNode.
    void SetActiveAttributeOnNode(std::string nodeID,
                                 std::string nodeType,
                                 std::string attributeName);
    ///Create a new assembly
    void CreateAssembly(std::string assemblyID);
   
    ///Create a new clone
    void CreateClone(std::string cloneID,
                    std::string originalID,
                    std::string orignalType);

    ///Create a new part
    void CreatePart(std::string fileName,
                   std::string partID,
                   std::string parentID);

    ///Clear out the associated information for a node
    ///\param nodeID The node id
    ///\param nodeTyp The type of node
    void RemoveNode(std::string nodeID,
                    std::string nodeType);

    ///Get a specified CADAttribute for a specified CADNode
    ///\param nodeID The CADNode  
    ///\param attributeName The name of the CADAttribute to find.
    ///\param component The name of the CADMaterial component to update.
    ///\param face The face to apply the update to.
    ///\param values The new values.
    void UpdateMaterialComponent(std::string nodeID,
                                             std::string attributeName,std::string component,
                                             std::string face,
                                             std::vector<double> values);

    ///Get a specified CADAttribute for a specified CADNode
    ///\param nodeID The CADNode  
    ///\param attributeName The name of the CADAttribute to find.
    ///\param type The type of mode to update.
    ///\param mode The new mode.
    void UpdateMaterialMode(std::string nodeID,
                                      std::string attributeName,
                                      std::string type,
                                      std::string mode);

    ///Get a specific part. 
    ///\param partID The ID of the part to search form
    VE_SceneGraph::CADEntity* GetPart(std::string partID);

    ///Get a specific assembly. 
    ///\param assemblyID The ID of the assembly to search form
    VE_SceneGraph::DCS* GetAssembly(std::string assemblyID);

    ///Get a specific assembly. 
    ///\param assemblyID The ID of the assembly to search form
    VE_SceneGraph::Clone* GetClone(std::string cloneID);

    ///\param cloneID The part ID to search for.
    bool CloneExists(std::string clone);

    ///\param partID The part ID to search for.
    bool PartExists(std::string partID);

    ///\param assemblyID The assembly ID to search for.
    bool AssemblyExists(std::string assemblyID);
    ///The current graph
    std::string GetRootCADNodeID();
    
    ///Equal operator
    ///\param rhs The ModelCADHandler we are setting this one equal to.
    ModelCADHandler& operator=(const ModelCADHandler& rhs);

    ///Not used
    virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray ){ return false; }
    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand(){ ; }

protected:
    ///A list of the current parts.
    std::map< std::string, VE_SceneGraph::CADEntity* > m_partList;
    ///A list of the current assemblies.
    std::map< std::string, osg::ref_ptr< VE_SceneGraph::DCS > > m_assemblyList;
    ///A list of clones.
    std::map< std::string, VE_SceneGraph::Clone* > m_cloneList;
    ///ID for root CAD node id
    std::string m_rootCADNodeID;
	///Attribute list mapping for all CAD
	std::map<std::string, osg::ref_ptr< osg::StateSet> > m_globalAttributeList;
#ifdef _OSG
    ///The map of node attributes.
    std::map< std::string, std::vector< 
        std::pair< std::string, osg::ref_ptr< osg::StateSet > > > 
        > m_nodeAttributes;
#endif
};
}
#endif // MODEL_CAD_HANDLER_H
