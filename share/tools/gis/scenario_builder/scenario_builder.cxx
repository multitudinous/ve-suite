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
#include <string>

#include "Scenario.h"

#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>

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
    if( argc < 1 )
    {
        std::cout << " What ! " << argv[ 0 ] << std::endl;
        return 1;
    }

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

    iaf::scenario::Scenario* tempScenario = new iaf::scenario::Scenario();
    tempScenario->SetLocation( "LocationOfAFarm" );
    tempScenario->SetRotation( "CG" );
    tempScenario->SetRemovalRate( "25-35" );
    tempScenario->SetClimateModel( "ClimateModelOfChoice" );
    tempScenario->SetTillage( "CH+FC" );

    tempScenario->GetXMLScenario();
    
    delete tempScenario;
    
    return 0;
}

