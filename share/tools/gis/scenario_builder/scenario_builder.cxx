/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <fstream>
#include <iostream>
#include <string>

#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/DataValuePairPtr.h>
/*
1 scenerio string
2. based on markup run create functions
3. xmlobject data transfer

- per shape file
-- how many objects do I have in the shape file
- user defined
-- how many objects do I have in the shape file

- xls file
-- farm location
-- rotation
-- tillage
-- removal rate
- climate 
-- model descriptor
- soil is soil
*/


XERCES_CPP_NAMESPACE_USE

int main( int argc, char* argv[] )
{
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch(const XMLException &toCatch)
    {
        std::cerr << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode(toCatch.getMessage()) << std::endl;
        return false;
    }
    
    ///Initialize VE-Open
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new ves::open::xml::XMLCreator() );


    ves::open::xml::CommandPtr scenarioCommand( new ves::open::xml::Command() );
    scenarioCommand->SetCommandName( "iaf scenario" );
    
    {
        ///Set the climate model
        ///I am not sure of the possible model choices here
        ves::open::xml::DataValuePairPtr climateDVP( new ves::open::xml::DataValuePair() );
        climateDVP->SetData( "climateModel", "LookUpTable" );
        scenarioCommand->AddDataValuePair( climateDVP );
    }
    
    {
        ///Set the GIS location
        ves::open::xml::DataValuePairPtr gisDVP( new ves::open::xml::DataValuePair() );
        gisDVP->SetData( "location", "FarmNameOrGISLocation" );
        scenarioCommand->AddDataValuePair( gisDVP );
    }
    
    {
        ///Set the management tools
        ves::open::xml::CommandPtr managementCommand( new ves::open::xml::Command() );
        managementCommand->SetCommandName( "managementInputs" );
        
        ///Rotation choices
        ///CG, SB, RCSB, RCHSB
        ///This shoudl actually be an enum OR'ed together
        ves::open::xml::DataValuePairPtr rotationDVP( new ves::open::xml::DataValuePair() );
        rotationDVP->SetData( "rotation", "CG" );
        managementCommand->AddDataValuePair( rotationDVP );
        
        ///Tillage choices
        ///CH+FC, NT, MP+FC
        ///This shoudl actually be an enum OR'ed together
        ves::open::xml::DataValuePairPtr tillageDVP( new ves::open::xml::DataValuePair() );
        tillageDVP->SetData( "tillage", "CH+FC" );
        managementCommand->AddDataValuePair( tillageDVP );
        
        ///Removal rate
        ///25-35, 40-50, 55-65, 70-80
        ///Maybe make this one an double?
        ves::open::xml::DataValuePairPtr removalRateDVP( new ves::open::xml::DataValuePair() );
        removalRateDVP->SetData( "removalRate", "25-35" );
        managementCommand->AddDataValuePair( removalRateDVP );
        
        ///Now add back into the scenarioCommand
        ves::open::xml::DataValuePairPtr managementDVP( new ves::open::xml::DataValuePair() );
        managementDVP->SetData( "management", managementCommand );
        scenarioCommand->AddDataValuePair( managementDVP );
    }

   
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( 
        scenarioCommand, "Command" ) );

    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    std::string filename( "test.vec" );
    netowrkWriter.WriteXMLDocument( nodes, filename, "Command" );
    
    return 0;
}

