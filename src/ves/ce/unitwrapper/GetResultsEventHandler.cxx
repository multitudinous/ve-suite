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
#include <ves/ce/unitwrapper/GetResultsEventHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/XMLReaderWriter.h>

using namespace VE_CE;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
GetResultsEventHandler::GetResultsEventHandler()
        : VE_CE::EventHandler()
{
    baseModel = ves::open::xml::model::ModelPtr();
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
GetResultsEventHandler::~GetResultsEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GetResultsEventHandler::SetBaseObject( ves::open::xml::XMLObjectPtr model )
{
    try
    {
        if( model )
        {
            baseModel = boost::dynamic_pointer_cast<ves::open::xml::model::Model>( model );
        }
    }
    catch ( ... )
    {
        baseModel = ves::open::xml::model::ModelPtr();
        std::cout << "Invalid object passed to SetInputsEventHandler::SetGlobalBaseObject!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
std::string GetResultsEventHandler::Execute( std::vector< ves::open::xml::XMLObjectPtr > objectToProcess )
{
    if( !baseModel )
    {
        std::cerr << "Must call GetResultsEventHandler::SetBaseObject first" 
            << std::endl;
        return std::string( "NULL" );
    }

    ves::open::xml::CommandPtr resultsCommand( new ves::open::xml::Command() );
    resultsCommand->SetCommandName( "Model Results" );

    const std::vector< ves::open::xml::CommandPtr > resultsVec = baseModel->GetResults();
    size_t numResults = resultsVec.size();

    if( numResults == 0 )
    {
        std::cout << "No results for this unit : " 
            << baseModel->GetVendorName() << std::endl;
        return std::string( "NULL" );
    }

    for( size_t i = 0; i < numResults; ++i )
    {
        ves::open::xml::CommandPtr tempResult = resultsVec.at( i );
        ves::open::xml::DataValuePairPtr tempPair( new ves::open::xml::DataValuePair() );
        tempPair->SetData( tempResult->GetCommandName(), tempResult );
        resultsCommand->AddDataValuePair( tempPair );
    }

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair < ves::open::xml::XMLObjectPtr,
                     std::string > ( resultsCommand, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    return status;
}
