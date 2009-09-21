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
#include <ves/xplorer/communication/CommandHandler.h>

#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

vprSingletonImp( ves::xplorer::CommandHandler );

using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
CommandHandler::CommandHandler():
        m_xplorer( 0 )
{
   m_input = ves::open::xml::CommandPtr();
}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::Initialize()
{}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::CleanUp()
{}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::InitScene()
{}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::PreFrameUpdate()
{}
////////////////////////////////////////////////////////////////////////////////
bool CommandHandler::SetXMLCommand( ves::open::xml::CommandPtr inputCommand )
{
    //The calling function is reponsible for deleting the incoming command
    //If in cluster mode
    if( !m_xplorer )
    {
        return false;
    }

    //Now send the data to xplorer
    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();

    // New need to destroy document and send it
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( inputCommand, "vecommand" ) );
    std::string xmlDocument( "returnString" );
    netowrkWriter.WriteXMLDocument( nodes, xmlDocument, "Command" );

    if( !CORBA::is_nil( m_xplorer->_this() ) && !xmlDocument.empty() )
    {
        try
        {
            //new way
            //std::cout << xmlDocument << std::endl;
            m_xplorer->SetXplorerData( xmlDocument );
        }
        catch ( ... )
        {
            return false;
        }
    }
    return true;
}
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::CommandPtr CommandHandler::GetXMLCommand()
{
    return m_input;
}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::SetXplorer( Body_VEXplorer_i* xplorer )
{
    m_xplorer = xplorer;
}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::SendConductorMessage( std::string message )
{
    //Now tell conductor to display text
    ves::open::xml::DataValuePairPtr dvp( new ves::open::xml::DataValuePair( std::string( "STRING" ) ) );
    dvp->SetData( "TEXT_OUTPUT", message );
    ves::open::xml::CommandPtr vec( new ves::open::xml::Command() );
    vec->SetCommandName( std::string( "TEXT_FEEDBACK" ) );
    vec->AddDataValuePair( dvp );
    SetXMLCommand( vec );
}
