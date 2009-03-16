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
#ifndef VE_XPLORER_MODEL_DATASET_HANDLER_H
#define VE_XPLORER_MODEL_DATASET_HANDLER_H

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
class DataSet;

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
/*!\file ModelDatasetHandler.h
Mananger for the Dataset associated with a Model
*/
/*!\class ves::xplorer::ModelDatasetHandler
*
*/
class VE_XPLORER_EXPORTS ModelDatasetHandler : public GlobalBase
{
public:
    ///Constructor
    ///\param rootCADNode The top-level CAD
    ModelDatasetHandler( ves::xplorer::scenegraph::DCS* rootCADNode );
    ///Copy Construtor
    ///\param rhs The ModelDatasetHandler we are copying.
    ModelDatasetHandler( const ModelDatasetHandler& rhs );

    ///Destructor
    virtual ~ModelDatasetHandler();

    ///Add a new attribute to a node
    ///\param nodeID The ID of the node to add Attribute to.
    ///\param nodeType The node type.
    ///\param attributeName The name of the CADAttribute to activate on the CADNode.
    void SetActiveDataset( std::string nodeID,
                                   std::string nodeType,
                                   std::string attributeName );

    ///Create a new part
    void CreateDataset( std::string fileName );

    ///Clear out the associated information for a node
    ///\param nodeID The node id
    ///\param nodeTyp The type of node
    void RemoveDataset( std::string nodeID );

    ///Get a specific assembly.
    ///\param assemblyID The ID of the assembly to search form
    ves::xplorer::DataSet* GetDataset( const std::string& assemblyID );

    ///Check to see if the assembly already exists and is loaded
    ///\param assemblyID The assembly ID to search for.
    bool DatasetExists( const std::string& assemblyID );

    ///Equal operator
    ///\param rhs The ModelDatasetHandler we are setting this one equal to.
    ModelDatasetHandler& operator=( const ModelDatasetHandler& rhs );

    ///Not used
    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand()
    {
        ;
    }

protected:
    ///A list of the current datasets.
    std::map< std::string, ves::xplorer::DataSet* > m_datasetList;
};
}
}
#endif // VE_XPLORER_MODEL_DATASET_HANDLER_H
