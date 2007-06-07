/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/CommandHandler.h"

#include "VE_Open/XML/XMLReaderWriter.h"

vprSingletonImp( VE_Xplorer::CommandHandler );

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
CommandHandler::CommandHandler()
{

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
    
    /*// New xml command queue
    if ( !commandVectorQueue.empty() )
    {
        std::vector< Command* >::iterator iter;
        iter = commandVectorQueue.begin();
        (*bufferCommand) = (*(*iter));
        delete commandVectorQueue.at( 0 );
        commandVectorQueue.erase( iter );
                 std::stringstream commandStatement;
                commandStatement<<"Executing: "<<bufferCommand->GetCommandName()<<std::endl;
                SetXplorerData(commandStatement.str());
    }
    else
    {
        ;
    }*/
    
    //Now send the data to xplorer
    VE_XML::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();

    // New need to destroy document and send it
    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( inputCommand, "vecommand" ) );
    std::string xmlDocument( "returnString" );
    netowrkWriter.WriteToString();
    netowrkWriter.WriteXMLDocument( nodes, xmlDocument, "Command" );

    if ( !CORBA::is_nil( m_xplorer->_this() ) && !xmlDocument.empty() )
    {
       try
       {
          // CORBA releases the allocated memory so we do not have to
          // Old way
          //vjobs->SetCommandString( xmlDocument.c_str() );
          //new way
          m_xplorer->SetCommand( xmlDocument.c_str() );
       }
       catch ( ... )
       {
          //VjObs::_tao_release( vjobs );
          //if ( !IsConnectedToXplorer() )
          {  
             //wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
             //              "Communication Failure", wxOK | wxICON_INFORMATION );
             return false;
          }
       }
    }
   //Clean up memory
   //delete veCommand;
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