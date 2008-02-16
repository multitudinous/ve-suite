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
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/event/cad/CADEventHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/scenegraph/UpdateIDOnChildrenVisitor.h>

#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADAttribute.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Command.h>

#include <ves/xplorer/Debug.h>

#include <boost/filesystem/path.hpp>
#include <iostream>

#ifdef _OSG
#include <osg/Node>
#endif
using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;
using namespace ves::xplorer::scenegraph;
//////////////////////////////////////////////////////////
///Constructor                                          //
//////////////////////////////////////////////////////////
CADEventHandler::CADEventHandler()
: ves::xplorer::event::EventHandler()
{
    m_cadNode = 0;
    m_activeModel = 0;
    m_cadHandler = 0;
}
////////////////////////////////////////////////////////////
CADEventHandler::CADEventHandler( const CADEventHandler& rhs )
        : ves::xplorer::event::EventHandler()
{
    m_cadNode = rhs.m_cadNode;
    m_activeModel = rhs.m_activeModel;
    m_cadHandler = rhs.m_cadHandler;
}
////////////////////////////////////
///Destructor                     //
////////////////////////////////////
CADEventHandler::~CADEventHandler()
{
    m_cadNode = 0;
    m_activeModel = 0;
    m_cadHandler = 0;
}
///////////////////////////////////////////////////////////////////////////
void CADEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* model )
{
    try
    {
        if( model )
        {
            m_activeModel = dynamic_cast<ves::xplorer::Model*>( model );
        }
        else
        {
            m_activeModel = ves::xplorer::ModelHandler::instance()->GetActiveModel();
        }
        if( m_activeModel )
        {
            m_cadHandler = m_activeModel->GetModelCADHandler();
        }
    }
    catch ( ... )
    {
        m_activeModel = 0;
        m_cadHandler = 0;
        std::cout << "Invalid object passed to CADEventHandler!" << std::endl;
    }
}
///////////////////////////////////////////////////////
///Exectute the event                                //
///////////////////////////////////////////////////////
void CADEventHandler::Execute( XMLObjectPtr veXMLObject )
{
    if( m_cadHandler && m_activeModel )
    {
        //this is overridden in derived classes
        _operateOnNode( veXMLObject );
    }
}
///////////////////////////////////////////////////////////////////////
CADEventHandler& CADEventHandler::operator=( const CADEventHandler& rhs )
{
    if( this != &rhs )
    {
        m_cadNode = rhs.m_cadNode;
        m_activeModel = rhs.m_activeModel;
        m_cadHandler = rhs.m_cadHandler;
    }
    return *this;
}
///////////////////////////////////////////////////////////////
void CADEventHandler::_setAttributesOnNode( CADNodePtr activeNode )
{
    //std::cout<<"Setting Attributes!!"<<std::endl;
    //set attributes
    size_t nAttributes = 0;
    nAttributes = activeNode->GetAttributeList().size();
    for( size_t i = 0;  i < nAttributes; i++ )
    {
        //This should change once the xml/cad stuff is committed
        CADAttributePtr currentAttribute = activeNode->GetAttribute( i );
        //  std::cout<<"Adding attribute: "<<currentAttribute.GetAttributeName()<<std::endl;
        m_cadHandler->AddAttributeToNode( activeNode->GetID(), currentAttribute );
    }
    if( nAttributes )
    {
        m_cadHandler->SetActiveAttributeOnNode( activeNode->GetID(),
                                                activeNode->GetNodeType(),
                                                activeNode->GetActiveAttribute()->GetAttributeName() );
    }


}
//////////////////////////////////////////////////////////////////
void CADEventHandler::_setTransformOnNode( CADNodePtr activeNode )
{
    //set the transform
    ves::xplorer::scenegraph::DCS* transform = 0;
    std::string nodeID = activeNode->GetID();
    if( activeNode->GetNodeType() == "Assembly" )
    {
        //std::cout<<"Setting transform on Assembly: "<<nodeID<<std::endl;
        transform = m_cadHandler->GetAssembly( nodeID );
    }
    else if( activeNode->GetNodeType() == "Part" )
    {
        //std::cout<<"Setting transform on Part: "<<nodeID<<std::endl;
        transform = m_cadHandler->GetPart( nodeID )->GetDCS();
    }
    else if( activeNode->GetNodeType() == "Clone" )
    {
        //std::cout<<"Setting transform on Clone: "<<nodeID<<std::endl;
        if( m_cadHandler->GetClone( nodeID ) )
        {
            transform = m_cadHandler->GetClone( nodeID )->GetClonedGraph();
        }
    }
    if( transform )
    {
        transform->SetTranslationArray( activeNode->GetTransform()->GetTranslationArray()->GetArray() );
        transform->SetRotationArray( activeNode->GetTransform()->GetRotationArray()->GetArray() );
        transform->SetScaleArray( activeNode->GetTransform()->GetScaleArray()->GetArray() );
    }
    else
    {
        std::cout << "No transform found!!" << std::endl;
    }
}
///////////////////////////////////////////////////////////////////////
void CADEventHandler::SetNodeDescriptors( std::string nodeID,
                                          std::string nodeType,
                                          std::string descriptorName,
                                          std::string descriptorValue )
{
    //set the uuid on the osg node so that we can get back to vexml
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( descriptorName );
    descriptorsList.push_back( descriptorValue );

    if( nodeType == "Assembly" )
    {
        ves::xplorer::scenegraph::DCS* assemblyNode = m_cadHandler->GetAssembly( nodeID );
        assemblyNode->setDescriptions( descriptorsList );
    }
    else if( nodeType == "Part" )
    {
        ves::xplorer::scenegraph::CADEntity* partNode = m_cadHandler->GetPart( nodeID );
        partNode->GetDCS()->setDescriptions( descriptorsList );
    }
    else if( nodeType == "Clone" )
    {
        ves::xplorer::scenegraph::Clone* cloneNode = m_cadHandler->GetClone( nodeID );
        ves::xplorer::scenegraph::UpdateIDOnChildrenVisitor idUpdate( cloneNode->GetClonedGraph(), descriptorValue );
        cloneNode->GetClonedGraph()->setDescriptions( descriptorsList );
    }
}
/////////////////////////////////////////////////////////////
void CADEventHandler::_addNodeToNode( std::string parentID,
                                      CADNodePtr activeNode )
{
    ves::xplorer::scenegraph::DCS* parentAssembly = 0;
    parentAssembly = m_cadHandler->GetAssembly( parentID );

    vprDEBUG( vesDBG, 1 ) << "|---Adding node to parent---" << parentID
    << std::endl << vprDEBUG_FLUSH;
    if( !parentAssembly )
    {
        std::cout << "|---No parent found---id " << parentID << std::endl;
        return;
    }

    if( activeNode->GetNodeType() == "Assembly" )
    {
        CADAssemblyPtr newAssembly = activeNode;
        vprDEBUG( vesDBG, 2 ) <<"|---Assembly---"<<std::endl<< vprDEBUG_FLUSH;
        vprDEBUG( vesDBG, 2 )<<"|\t---"<<newAssembly->GetID()<<"---"
            <<std::endl<< vprDEBUG_FLUSH;
        vprDEBUG( vesDBG, 2 )<<"|\t---"<<newAssembly->GetNodeName()
            <<"---"<<std::endl<< vprDEBUG_FLUSH;
        vprDEBUG( vesDBG, 2 )<<"|\t--- ("<<newAssembly->GetNumberOfChildren()
            <<") child nodes---"<<std::endl<< vprDEBUG_FLUSH;

        m_cadHandler->CreateAssembly( newAssembly->GetID() );
        m_cadHandler->GetAssembly( newAssembly->GetID() )->
            SetName( newAssembly->GetNodeName() );

        vprDEBUG( vesDBG, 2 )<<"|\t---Setting node properties---"
            <<std::endl<< vprDEBUG_FLUSH;

        _setTransformOnNode( newAssembly );
        vprDEBUG( vesDBG, 2 )<<"|\t---Set transform---"
            <<std::endl<< vprDEBUG_FLUSH;

        _setAttributesOnNode( newAssembly );
        vprDEBUG( vesDBG, 2 )<<"|\t---Set Attributes---"
            <<std::endl<< vprDEBUG_FLUSH;
        parentAssembly->AddChild( m_cadHandler->GetAssembly( newAssembly->GetID() ) );

        unsigned int nChildren = newAssembly->GetNumberOfChildren();
        for( unsigned int i = 0; i < nChildren; i++ )
        {
            vprDEBUG( vesDBG, 2 )<<"|\tAdding child: "
                <<newAssembly->GetChild(i)->GetNodeName()
                <<std::endl<< vprDEBUG_FLUSH;
            _addNodeToNode( newAssembly->GetID(), newAssembly->GetChild( i ) );
        }
        m_cadHandler->GetAssembly( newAssembly->GetID() )->ToggleDisplay( newAssembly->GetVisibility() );
        SetNodeDescriptors( newAssembly->GetID(), "Assembly", "VE_XML_ID", newAssembly->GetID() );
        //Set a default material on nodes that have no initial material
        ves::xplorer::scenegraph::util::MaterialInitializer material_initializer( m_cadHandler->GetAssembly( newAssembly->GetID() ) );
    }
    else if( activeNode->GetNodeType() == "Part" )
    {
        CADPartPtr newPart = activeNode;
        vprDEBUG( vesDBG, 1 ) << "|\t---Part---"
            << std::endl << vprDEBUG_FLUSH;
        vprDEBUG( vesDBG, 1 ) << "|\t---" << newPart->GetID()
            << "---" << std::endl << vprDEBUG_FLUSH;
        std::string tempFilename = newPart->GetCADFileName();
        boost::filesystem::path correctedPath( newPart->GetCADFileName(), boost::filesystem::no_check );
        vprDEBUG( vesDBG, 1 ) << "|\t---" << tempFilename << "---"
            << correctedPath.native_file_string()
            << std::endl << vprDEBUG_FLUSH;
        m_cadHandler->CreatePart( correctedPath.native_file_string(),
                                  newPart->GetID(),
                                  parentID );

        ves::xplorer::scenegraph::CADEntity* partNode = m_cadHandler->GetPart( newPart->GetID() );
        if( partNode->GetNode()->GetNode() )
        {
            partNode->GetNode()->SetName( newPart->GetNodeName() );
            partNode->GetDCS()->setName( newPart->GetNodeName() );

            //set the visibility
            partNode->GetDCS()->ToggleDisplay( newPart->GetVisibility() );

            if( newPart->HasPhysics() )
            {
                partNode->InitPhysics();

                partNode->GetPhysicsRigidBody()->SetMass( newPart->GetMass() );
                partNode->GetPhysicsRigidBody()->setFriction( newPart->GetFriction() );
                partNode->GetPhysicsRigidBody()->setRestitution( newPart->GetRestitution() );

                std::string physicsMesh = newPart->GetPhysicsMesh();
                if( physicsMesh == "Bounding Box" )
                {
                    partNode->GetPhysicsRigidBody()->BoundingBoxShape();
                }
                else if( physicsMesh == "Convex" )
                {
                    partNode->GetPhysicsRigidBody()->ConvexShape();
                }
                else if( physicsMesh == "Static Concave" )
                {
                    partNode->GetPhysicsRigidBody()->StaticConcaveShape();
                }
            }

            vprDEBUG( vesDBG, 1 ) << "|\t---Setting node properties---" << std::endl << vprDEBUG_FLUSH;
            _setTransformOnNode( newPart );
            vprDEBUG( vesDBG, 1 ) << "|\t---Set transform---" << std::endl << vprDEBUG_FLUSH;
            _setAttributesOnNode( newPart );
            vprDEBUG( vesDBG, 1 ) << "|\t---Set Attributes---" << std::endl << vprDEBUG_FLUSH;

            //set the uuid on the osg node so that we can get back to vexml
            SetNodeDescriptors( newPart->GetID(), "Part", "VE_XML_ID", newPart->GetID() );
            //Set a default material on nodes that have no initial material
            ves::xplorer::scenegraph::util::MaterialInitializer material_initializer( partNode->GetDCS() );
        }
        else
        {
            std::cerr << "|\t---ERROR: (CADEventHandler::_addNodeToNode) Unable to load file name: "
            << correctedPath.native_file_string() << std::endl;
        }
    }
}
