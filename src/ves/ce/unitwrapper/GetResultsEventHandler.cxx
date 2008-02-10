/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
    baseModel = 0;
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
            baseModel = dynamic_cast< ves::open::xml::model::ModelPtr >( model );
        }
    }
    catch ( ... )
    {
        baseModel = 0;
        std::cout << "Invalid object passed to SetInputsEventHandler::SetGlobalBaseObject!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
std::string GetResultsEventHandler::Execute( std::vector< ves::open::xml::XMLObjectPtr > objectToProcess )
{
    if( !baseModel )
    {
        std::cerr << "Must call GetResultsEventHandler::SetBaseObject first" << std::endl;
        return std::string( "NULL" );
    }

    ves::open::xml::Command resultsCommand;
    resultsCommand.SetCommandName( "Model Results" );

    size_t numInputs = baseModel->GetNumberOfResults();
    if( numInputs == 0 )
    {
        return std::string( "NULL" );
    }

    for( size_t i = 0; i < numInputs; ++i )
    {
        ves::open::xml::CommandPtr tempResult = baseModel->GetResult( i );
        ves::open::xml::DataValuePairWeakPtr tempPair = new ves::open::xml::DataValuePair();
        tempPair->SetData( tempResult->GetCommandName(), tempResult );
        tempResult->AddDataValuePair( tempPair );
    }

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair < ves::open::xml::XMLObjectPtr,
                     std::string > ( &resultsCommand, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    return status;
}
