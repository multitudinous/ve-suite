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
#include <ves/xplorer/event/cad/CADAddNodeEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/model/Model.h>

#include <iostream>
using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;

/*
 * We're keeping this class rather than moving AddNewNode into CADSlots because
 * AddNewNode relies on methods that live in the base class CADEventHandler.
 * 2013-07-05
 */
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADAddNodeEventHandler::CADAddNodeEventHandler()
    :
    ves::xplorer::event::CADEventHandler()
{
    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_CADNodeAdded ),
        "CADEventHandler.NodeAdded" );

    CONNECTSIGNALS_1( "%CADAddNewNode",
                      void( CADNodePtr ),
                      &CADAddNodeEventHandler::AddNewNode,
                      m_connections, any_SignalType, normal_Priority );
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
////////////////////////////////////////////////////////////////////////////////
// This method can go away once we're sure there is no calling path: 2013-07-05
void CADAddNodeEventHandler::_operateOnNode( XMLObjectPtr xmlObject )
{
    CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
    DataValuePairPtr cadNode = command->GetDataValuePair( "New Node" );
    CADNodePtr tempNode( boost::dynamic_pointer_cast<CADNode>( cadNode->GetDataXMLObject() ) );
    AddNewNode( tempNode );
}
////////////////////////////////////////////////////////////////////////////////
void CADAddNodeEventHandler::AddNewNode( ves::open::xml::cad::CADNodePtr cadNode )
{

    if( !m_activeModel )
    {
        m_activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        if( !m_activeModel )
        {
            return;
        }
    }


    if( !m_cadHandler )
    {
        m_cadHandler = m_activeModel->GetModelCADHandler();
        if( !m_cadHandler )
        {
            return;
        }
    }


    try
    {
        std::string nodeType =  cadNode->GetNodeType();

        //Get the xml model pointer so that the pointers used by the cad handler
        //are in the same memory space as the xml model in the ves::xplorer::Model.
        ves::open::xml::model::ModelPtr tempXmlModel = m_activeModel->GetModelData();
        CADAssemblyPtr rootAssembly =
            boost::dynamic_pointer_cast<CADAssembly>( tempXmlModel->GetGeometry() );

        CADNodePtr node;
        CADAssemblyPtr assembly;
        CADPartPtr part;

        if( nodeType == "Assembly" )
        {
            if( cadNode->GetID() == rootAssembly->GetID() )
            {
                assembly = rootAssembly;
            }
            else
            {
                assembly =
                    boost::dynamic_pointer_cast< CADAssembly >(
                        rootAssembly->SearchAllChildren( cadNode->GetID() ) );
            }

            node = boost::dynamic_pointer_cast<CADNode>( assembly );

            if( m_cadHandler->AssemblyExists( node->GetID() ) )
            {
                throw( "Assembly already exists" );
            }
        }
        else if( nodeType == "Part" )
        {
            part =
                boost::dynamic_pointer_cast<CADPart>(
                    rootAssembly->SearchAllChildren( cadNode->GetID() ) );
            node = boost::dynamic_pointer_cast<CADNode>( part );

            if( m_cadHandler->PartExists( node->GetID() ) )
            {
                throw( "Part already exists" );
            }
        }

        ///This is the root
        if( node->GetParent().empty() || ( node->GetParent() == "rootNode" ) )
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
            m_CADNodeAdded.signal( "" );
        }
        else if( nodeType == "Part" )
        {
            _addNodeToNode( node->GetParent(), part );
            m_CADNodeAdded.signal( part->GetCADFileName() );
        }

        // Emit signal saying we've just added a CAD node of some sort.
        //m_CADNodeAdded.signal( node->GetNodeName() );
    }
    catch( char* str )
    {
        std::cout << str << std::endl;
    }
    catch( ... )
    {
        std::cerr << "CADAddNodeEventHandler::AddNewNode : There was an "
                  << "exception generated somewhere in the CAD event handlers."
                  << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
