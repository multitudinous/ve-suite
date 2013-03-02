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
#include <ves/xplorer/ModelDatasetHandler.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/scenegraph/util/Attribute.h>
#include <ves/xplorer/scenegraph/DCS.h>

///This must be here due to boost header conflicts on windows
#include <ves/xplorer/Debug.h>

#include <latticefx/core/vtk/DataSet.h>

namespace ves
{
namespace xplorer
{
ModelDatasetHandler::ModelDatasetHandler( ves::xplorer::scenegraph::DCS* )
    :
    GlobalBase()
{
}
/////////////////////////////////////////////////////////////////////////////////////////////
ModelDatasetHandler::ModelDatasetHandler( const ModelDatasetHandler& rhs )
    :
    GlobalBase( rhs )
{
    m_datasetList = rhs.m_datasetList;
}
/////////////////////////////////////////////////////////////////////////////////////////////
ModelDatasetHandler::~ModelDatasetHandler()
{
    m_datasetList.clear();
}
/////////////////////////////////////////////////////////////////////////////////////////////
ModelDatasetHandler&
ModelDatasetHandler::operator=( const ModelDatasetHandler& rhs )
{
    if( this != &rhs )
    {
        m_datasetList = rhs.m_datasetList;
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelDatasetHandler::CreateDataset( std::string assemblyID )
{
    m_datasetList[ assemblyID ] = lfx::core::vtk::DataSetPtr( new lfx::core::vtk::DataSet() );
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelDatasetHandler::RemoveDataset( std::string )
{
    ;
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelDatasetHandler::SetActiveDataset( std::string, std::string, std::string )
{
}
/////////////////////////////////////////////////////////////////////////////////////////////
lfx::core::vtk::DataSetPtr ModelDatasetHandler::GetDataset( const std::string& partID )
{
    return m_datasetList[ partID ];
}
/////////////////////////////////////////////////////////////////////////////////////////////
bool ModelDatasetHandler::DatasetExists( const std::string& partID )
{
    std::map<std::string, lfx::core::vtk::DataSetPtr >::iterator foundPart;
    foundPart = m_datasetList.find( partID );

    if( foundPart != m_datasetList.end() )
    {
        return true;
    }
    return false;
}
/////////////////////////////////////////////////////////////////////////////////////////////
} // end xplorer
} // end ves
