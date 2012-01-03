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
#include <ves/ce/unitwrapper/SetInputsEventHandler.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Model.h>

using namespace VE_CE;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
SetInputsEventHandler::SetInputsEventHandler()
        : VE_CE::EventHandler()
{
    baseModel = ves::open::xml::model::ModelPtr();
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
SetInputsEventHandler::~SetInputsEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SetInputsEventHandler::SetBaseObject( ves::open::xml::XMLObjectPtr model )
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
std::string SetInputsEventHandler::Execute( std::vector< ves::open::xml::XMLObjectPtr > objectToProcess )
{
    if( !baseModel )
    {
        std::cerr << "Must call SetInputsEventHandler::SetBaseObject first" << std::endl;
        return std::string();
    }

    try
    {

        size_t numInputs = objectToProcess.size();
        for( size_t i = 0; i < numInputs; ++i )
        {
            ves::open::xml::CommandPtr command =  boost::dynamic_pointer_cast<ves::open::xml::Command>( objectToProcess.at( i ) );
            std::string dataName = command->GetCommandName();
            //ves::open::xml::CommandPtr tempInput = baseModel->GetInput( dataName );
            baseModel->SetInput( command );
            /*if( tempInput )
            {
                ( *tempInput ) = ( *command );
            }
            else
            {
                *( baseModel->GetInput() ) = ( *command );
            }*/
        }
    }
    catch ( ... )
    {
        std::cerr << " ERROR : SetInputsEventHandler::Execute " << std::endl;
    }
    return std::string();
}
