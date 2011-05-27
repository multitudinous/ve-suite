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
#include <ves/xplorer/event/cad/CADAddNodeEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/eventmanager/EventManager.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>

#include <iostream>
using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADAddNodeEventHandler::CADAddNodeEventHandler()
        : ves::xplorer::event::CADEventHandler()
{
    eventmanager::EventManager::instance()->RegisterSignal(
       new eventmanager::SignalWrapper< ves::util::StringSignal_type >( &m_CADNodeAdded ),
       "CADEventHandler.NodeAdded" );
}
///////////////////////////////////////////////////////////////////////////////////////
CADAddNodeEventHandler::CADAddNodeEventHandler( const CADAddNodeEventHandler& rhs )
        : ves::xplorer::event::CADEventHandler( rhs )
{}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADAddNodeEventHandler::~CADAddNodeEventHandler()
{}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADAddNodeEventHandler& CADAddNodeEventHandler::operator=( const CADAddNodeEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::CADEventHandler::operator=( rhs );
    }
    return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADAddNodeEventHandler::_operateOnNode( XMLObjectPtr xmlObject )
{
    try
    {
        CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
        DataValuePairPtr cadNode = command->GetDataValuePair( "New Node" );
        CADNodePtr tempNode( boost::dynamic_pointer_cast<CADNode>( cadNode->GetDataXMLObject() ) );
        std::string nodeType =  tempNode->GetNodeType();

        CADNodePtr node;
        CADAssemblyPtr assembly;
        CADPartPtr part;
        //ves::xplorer::scenegraph::DCS* parentAssembly = 0;

        if( nodeType == "Assembly" )
        {

            assembly = boost::dynamic_pointer_cast<CADAssembly>( cadNode->GetDataXMLObject() );
            node = boost::dynamic_pointer_cast<CADNode>( assembly );
            if( m_cadHandler->AssemblyExists( node->GetID() ) )
            {
                throw( "Assembly already exists" );
            }
        }
        else if( nodeType == "Part" )
        {
            part = boost::dynamic_pointer_cast<CADPart>( cadNode->GetDataXMLObject() );
            node = boost::dynamic_pointer_cast<CADNode>( part );

            if( m_cadHandler->PartExists( node->GetID() ) )
            {
                throw( "Part already exists" );
            }
        }

        ///This is the root
        if( node->GetParent().empty() )
        {
            ///add the root to the VEBaseClass DCS
            node->SetParent( "rootNode" );
            m_cadHandler->SetRootCADNodeID( node->GetID() );
        }
        else
        {
            //need to check if parent is on the graph already
            if( !m_cadHandler->AssemblyExists( node->GetParent() ) )
            {
                m_cadHandler->CreateAssembly( node->GetParent() );
                //We have to initialize some properties on the root node since
                //we are not creating it from xml data.
                //From intial creation, the top level node is called Model_Geometry in the GUI.
                //After that, CADSetNameEH handles the name appropriately.
                m_cadHandler->GetAssembly( node->GetParent() )->setName( "Model_Geometry" );

                //update the top level node descriptors
                SetNodeDescriptors( node->GetParent(), "Assembly", "VE_XML_ID", node->GetParent(), CADNodePtr() );
                //Add the top level CAD to the VEBaseClass
                m_activeModel->GetDCS()->addChild( m_cadHandler->GetAssembly( node->GetParent() ) );
            }
        }

        if( nodeType == "Assembly" )
        {
            _addNodeToNode( node->GetParent(), assembly );
            // Since assemblies don't strictly have an associated filename,
            // we send a blank filename arg.
            m_CADNodeAdded( "" );
        }
        else if( nodeType == "Part" )
        {
            _addNodeToNode( node->GetParent(), part );
            m_CADNodeAdded( part->GetCADFileName() );
        }

        // Emit signal saying we've just added a CAD node of some sort.
        //m_CADNodeAdded( node->GetNodeName() );
    }
    catch( char* str )
    {
        std::cout << str << std::endl;
    }
    catch ( ... )
    {
        std::cerr << "CADAddNodeEventHandler::_operateOnNode : There was an " 
            << "exception generated somewhere in the CAD event handlers." 
            << std::endl;
    }
}
