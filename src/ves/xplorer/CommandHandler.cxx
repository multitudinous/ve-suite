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
#if defined(WIN32)
    #define WIN32_LEAN_AND_MEAN
#endif
#include "VE_Xplorer/XplorerHandlers/CommandHandler.h"

#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"

vprSingletonImp( VE_Xplorer::CommandHandler );

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
CommandHandler::CommandHandler():
m_xplorer( 0 ),
m_input( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::Initialize()
{

}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::CleanUp()
{

}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::InitScene()
{

}
////////////////////////////////////////////////////////////////////////////////
void CommandHandler::PreFrameUpdate()
{

}
////////////////////////////////////////////////////////////////////////////////
bool CommandHandler::SetXMLCommand( VE_XML::Command* inputCommand )
{
    //The calling function is reponsible for deleting the incoming command
    //If in cluster mode
    if( !m_xplorer )
    {
        return false;
    }
    
    //Now send the data to xplorer
    VE_XML::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();

    // New need to destroy document and send it
    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( inputCommand, "vecommand" ) );
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
VE_XML::Command* CommandHandler::GetXMLCommand()
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
    VE_XML::DataValuePair* dvp = 
        new VE_XML::DataValuePair(  std::string("STRING") );
    dvp->SetData( "TEXT_OUTPUT", message );
    VE_XML::Command vec;
    vec.SetCommandName( std::string("TEXT_FEEDBACK") );
    vec.AddDataValuePair( dvp );
    SetXMLCommand( &vec );
}
