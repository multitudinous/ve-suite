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
 * Date modified: $Date: 2007-05-15 21:12:52 -0500 (Tue, 15 May 2007) $
 * Version:       $Rev: 7655 $
 * Author:        $Author: mccdo $
 * Id:            $Id: ModelCADHandler.cxx 7655 2007-05-16 02:12:52Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/ModelCADHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/SceneGraph/Utilities/Attribute.h"
#include "VE_Xplorer/SceneGraph/Clone.h"
#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/CADEntity.h"
#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"

#include "VE_Xplorer/SceneGraph/Utilities/OpacityVisitor.h"

#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAttribute.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/Socket/SocketAcceptor.h>
#include <vpr/System.h>

#include <osg/BlendFunc>
using namespace VE_Xplorer;
/////////////////////////////////////////////////////////////////////////////////////////////
ModelCADHandler::ModelCADHandler(VE_SceneGraph::DCS* rootNode)
{
    m_assemblyList["rootNode"] = rootNode;
}
/////////////////////////////////////////////////////////////////////////////////////////////
ModelCADHandler::ModelCADHandler(const ModelCADHandler& rhs)
{
    m_cloneList = rhs.m_cloneList;
    m_partList = rhs.m_partList;
    m_assemblyList = rhs.m_assemblyList;
    m_rootCADNodeID = rhs.m_rootCADNodeID;
    m_nodeAttributes = rhs.m_nodeAttributes;
}
/////////////////////////////////////////////////////////////////////////////////////////////
ModelCADHandler::~ModelCADHandler()
{
    std::map<std::string, VE_SceneGraph::CADEntity*>::iterator iter;
    for ( iter = m_partList.begin(); iter != m_partList.end(); iter++ )
    {
        cfdModelHandler::instance()->UnregisterCADFile( iter->second );
        delete iter->second;
    }
    m_partList.clear();

    ///Assembly list is map of dcs's which are osg smart pointers
    m_assemblyList.clear();

    for (std::map< std::string, 
         VE_SceneGraph::Clone* >::iterator itr = m_cloneList.begin();
         itr != m_cloneList.end(); itr++)
    {
        delete itr->second;
    }
    m_cloneList.clear();
    m_nodeAttributes.clear();
}
/////////////////////////////////////////////////////////////////////////////////////////////
ModelCADHandler& 
ModelCADHandler::operator=( const ModelCADHandler& rhs )
{
    if( this != &rhs )
    {
        m_cloneList = rhs.m_cloneList;
        m_partList = rhs.m_partList;
        m_assemblyList = rhs.m_assemblyList;
        m_rootCADNodeID = rhs.m_rootCADNodeID;
        m_nodeAttributes = rhs.m_nodeAttributes;
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::SetRootCADNodeID(std::string rootNodeId )
{
    m_rootCADNodeID = rootNodeId;
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::CreateClone(std::string cloneID, 
                                               std::string originalID, 
                                               std::string originalType )
{
    if(originalType == std::string( "Assembly" ) )
    {
      
        if ( GetAssembly( originalID ) )
        {
            m_cloneList[ cloneID ] = 
                new VE_SceneGraph::Clone( GetAssembly( originalID ) );
        }
    }
    else if( originalType == std::string( "Part" ) )
    {
        if ( GetPart( originalID ) )
        {
            m_cloneList[ cloneID ] = 
                new VE_SceneGraph::Clone( GetPart(originalID )->GetNode()->GetNode() );
        }
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::CreateAssembly( std::string assemblyID )
{
    m_assemblyList[ assemblyID ] = new VE_SceneGraph::DCS();
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::CreatePart( std::string fileName, 
                                              std::string partID, 
                                              std::string parentID )
{
    VE_SceneGraph::CADEntity* tempCAD = 
        cfdModelHandler::instance()->IsCADFileLoaded( fileName );
    if( tempCAD )
    {
        ///If we have already loaded the parts
        VE_SceneGraph::CADEntityHelper* tempNode = tempCAD->GetNode();
        m_partList[ partID ] = 
            new VE_SceneGraph::CADEntity( tempNode, 
                                          m_assemblyList[ parentID ] );
        vprDEBUG(vesDBG,1) << "|\t--Cloned new part--"
                            << std::endl << vprDEBUG_FLUSH;
    }
    else
    {
        ///If we have not loaded this part
        m_partList[ partID ] = 
            new VE_SceneGraph::CADEntity( fileName, m_assemblyList[ parentID ] );
        vprDEBUG(vesDBG,1) << "|\t--Loaded new part--" 
                            << std::endl << vprDEBUG_FLUSH;
    }
    cfdModelHandler::instance()->RegisterCADFile( m_partList[ partID ] );
    //add key pointer to physics map for bullet rigid body
    //add data pair for transform node
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::SetActiveAttributeOnNode(std::string nodeID, 
                                                                 std::string nodeType, 
                                                                 std::string attributeName )
{
#ifdef _OSG
    std::map<std::string, 
                 std::vector< std::pair< std::string, 
                                             osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find(nodeID);
   
    if(attributeList != m_nodeAttributes.end())
    {

        std::vector< std::pair<std::string,
                                   osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
        std::vector< std::pair<std::string,
                                   osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
        namesAndAttributes = attributeList->second;
        for(foundAttribute = namesAndAttributes.begin();
             foundAttribute != namesAndAttributes.end();
             foundAttribute++)
        {
            vprDEBUG(vesDBG,1) <<"|\tFound attribute: "
                                      <<foundAttribute->first
                                      <<std::endl<< vprDEBUG_FLUSH;
            if(foundAttribute->first == attributeName)
           {
               if(nodeType == "Assembly")
               {
                   vprDEBUG(vesDBG,1) <<"|\tSetting Assembly attribute: "
                                             <<foundAttribute->first
                                             <<std::endl<< vprDEBUG_FLUSH;
                   GetAssembly(nodeID)->setStateSet(foundAttribute->second.get());
                   return;
                }
                else if(nodeType == "Part")
                {
                   vprDEBUG(vesDBG,1) <<"|\tSetting Part attribute: "
                                             <<foundAttribute->first
                                             <<std::endl<< vprDEBUG_FLUSH;
                   GetPart(nodeID)->GetDCS()->setStateSet(foundAttribute->second.get());
                   vprDEBUG(vesDBG,1) <<"|\tvalid: "
                                             <<foundAttribute->first<<std::endl
                                             << vprDEBUG_FLUSH;
                   return;
                }
                else if(nodeType == "Clone")
                {
                   vprDEBUG(vesDBG,1) <<"|\tSetting Clone attribute: "
                                             <<foundAttribute->first
                                             <<std::endl
                                             << vprDEBUG_FLUSH;
                   GetClone(nodeID)->
                       GetClonedGraph()->setStateSet(foundAttribute->second.get());
                   return;
                }
            }
        }
    }
    else
    {
        vprDEBUG(vesDBG,1) <<"|\tAttribute not found on node: "
                                   <<attributeName<<std::endl
                                   << vprDEBUG_FLUSH;
    }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::MakeCADRootTransparent()
{
   if(!AssemblyExists(m_rootCADNodeID))
   {
	   return;
   }
#ifdef _OSG

   osg::ref_ptr< osg::StateSet > attribute = new osg::StateSet;
   osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc;

   bf->setFunction( osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
   attribute->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );
   attribute->setRenderBinDetails( 99, std::string( "DepthSortedBin" ) );
   attribute->setMode( GL_BLEND, osg::StateAttribute::ON );
   attribute->setAttributeAndModes( bf.get(), osg::StateAttribute::ON );

   try
   {
      m_assemblyList[m_rootCADNodeID]->setStateSet( attribute.get() );
      VE_SceneGraph::Utilities::OpacityVisitor 
          opacity_visitor( m_assemblyList[m_rootCADNodeID], true );
   }

   catch(...)
   {
      vprDEBUG(vesDBG,1) <<"|\tRoot CADNode not found!!!"
                                 <<std::endl
                                 << vprDEBUG_FLUSH;
      vprDEBUG(vesDBG,1) <<"|\tModelCADHandler::MakeCADRootTransparent()---"
                                 <<std::endl<< vprDEBUG_FLUSH;
   }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::MakeCADRootOpaque()
{
   if( !AssemblyExists( m_rootCADNodeID ) )
   {
	   return;
   }
#ifdef _OSG
   try
   {
      if( m_assemblyList[m_rootCADNodeID]->getStateSet() )
      {   
         m_assemblyList[ m_rootCADNodeID ]->getStateSet()->clear();
         VE_SceneGraph::Utilities::OpacityVisitor 
             opacity_visitor( m_assemblyList[ m_rootCADNodeID ], false );
      }
   }
   catch(...)
   {
      vprDEBUG(vesDBG,1) <<"|\tRoot CADNode not found!!!"
                                <<std::endl
                                << vprDEBUG_FLUSH;
      vprDEBUG(vesDBG,1) <<"|\tModelCADHandler::MakeCADRootOpaque()---"
                                <<std::endl<< vprDEBUG_FLUSH;
   }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::RemoveAttributeFromNode(std::string nodeID,
                                                                  std::string nodeType,
                                                                  std::string attributeName)
{
#ifdef _OSG
	std::map< std::string, 
                 std::vector< std::pair< std::string, 
                                             osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find( nodeID );
   
   
    if(attributeList != m_nodeAttributes.end())
    {
        std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
        std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
        namesAndAttributes = attributeList->second;
        for(foundAttribute = namesAndAttributes.begin();
            foundAttribute != namesAndAttributes.end();)
        {
            vprDEBUG(vesDBG,1) <<"|\tFound attribute: "
                                      <<foundAttribute->first
                                      <<std::endl<< vprDEBUG_FLUSH;
            if( foundAttribute->first == attributeName )
            {
                namesAndAttributes.erase( foundAttribute );
        		   
                if( nodeType == "Assembly")
                {
                    GetAssembly( nodeID )->getStateSet()->clear();
                }
                else if( nodeType == "Part" )
                {
                    GetPart( nodeID )->GetDCS()->getStateSet()->clear();
                }
                else if( nodeType == "Clone")
                {
                   GetClone( nodeID )->GetClonedGraph()->getStateSet()->clear();
                }
                break;
            }
            foundAttribute++;
        }
    }
    else
    {
        vprDEBUG(vesDBG,1) <<"|\tAttribute not found: "
                                  <<attributeName<<std::endl
                                  << vprDEBUG_FLUSH;
    }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::AddAttributeToNode(std::string nodeID,
                                                          VE_XML::VE_CAD::CADAttribute* 
                                                          newAttribute)
{
#ifdef _OSG
    vprDEBUG(vesDBG,1) <<"|\tModelCADHandler::AddAttributeToNode()---"<<std::endl<< vprDEBUG_FLUSH;
    osg::ref_ptr<VE_SceneGraph::Utilities::Attribute> attribute = new VE_SceneGraph::Utilities::Attribute();
    attribute->CreateStateSetFromAttribute(newAttribute);

    std::pair<std::string,osg::ref_ptr< osg::StateSet > >attributeInfo;
    attributeInfo.first = newAttribute->GetAttributeName();
    attributeInfo.second = attribute.get();

    std::map< std::string, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find(nodeID);

    if(attributeList != m_nodeAttributes.end())
    {
        vprDEBUG(vesDBG,1) <<"|\tAdding attribute: "<<attributeList->first<<std::endl<< vprDEBUG_FLUSH;
        attributeList->second.push_back(attributeInfo);
    }
    else
    { 
        std::vector< std::pair<std::string,
                                    osg::ref_ptr< osg::StateSet > > > temp;
        temp.push_back( attributeInfo );
        m_nodeAttributes[ nodeID ] = temp;

        //create the empty state set to restore defaults
        std::pair<std::string,osg::ref_ptr< osg::StateSet > >defaultAttributeInfo;
        defaultAttributeInfo.first = "Default Attribute";
        defaultAttributeInfo.second = new osg::StateSet();
        m_nodeAttributes[ nodeID ].push_back( defaultAttributeInfo );
    }
    vprDEBUG( vesDBG,1 ) <<"|\tend ModelCADHandler::AddAttributeToNode()---"
                                <<std::endl<< vprDEBUG_FLUSH;
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::UpdateMaterialMode(std::string nodeID,
                                                          std::string attributeName,
                                                          std::string type,
                                                          std::string mode)
{
#ifdef _OSG
    std::map< std::string, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find(nodeID);
   
    if(attributeList != m_nodeAttributes.end())
    {
        std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
        std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
        namesAndAttributes = attributeList->second;
        for(foundAttribute = namesAndAttributes.begin();
            foundAttribute != namesAndAttributes.end();
            foundAttribute++)
        {
            vprDEBUG(vesDBG,1) <<"|\tFound attribute: "<<foundAttribute->first<<std::endl<< vprDEBUG_FLUSH;
            if(foundAttribute->first == attributeName)
            {
                ///update the material component
                osg::ref_ptr<VE_SceneGraph::Utilities::Attribute> attribute = 
                              dynamic_cast<VE_SceneGraph::Utilities::Attribute*>(foundAttribute->second.get());
                if(attribute.valid())
                {
                    attribute->UpdateMaterialMode(type,mode);
                }
                else
                {
                    vprDEBUG(vesDBG,1) <<"|\tAttribute not found: "<<attributeName<<std::endl<< vprDEBUG_FLUSH;
                }
            }
        }
    }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::UpdateMaterialComponent(std::string nodeID, 
                                                                 std::string attributeName,
                                                                 std::string component,
                                                                 std::string face,
                                                                 std::vector<double> values)
{
#ifdef _OSG
   std::map< std::string, 
                std::vector< std::pair< std::string, 
                                osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
   attributeList = m_nodeAttributes.find(nodeID);
   
    if(attributeList != m_nodeAttributes.end())
    {
        std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
        std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
        namesAndAttributes = attributeList->second;
        for(foundAttribute = namesAndAttributes.begin();
            foundAttribute != namesAndAttributes.end();
            foundAttribute++)
        {
             vprDEBUG(vesDBG,1) <<"|\tFound attribute: "
                                       <<foundAttribute->first
                                       <<std::endl<< vprDEBUG_FLUSH;
             if(foundAttribute->first == attributeName)
             {
                ///update the material component
                osg::ref_ptr<VE_SceneGraph::Utilities::Attribute> attribute = 
                              dynamic_cast<VE_SceneGraph::Utilities::Attribute*>
                                ( foundAttribute->second.get() );
                if( attribute.valid() )
                {
                    attribute->UpdateMaterial(component, face, values);
                }
                else
                {
                    vprDEBUG(vesDBG,1) <<"|\tAttribute not found: "<<attributeName<<std::endl<< vprDEBUG_FLUSH;
                }
            }
        }
    }
    else
    {
        vprDEBUG(vesDBG,1) <<"|\tAttribute not found: "<<attributeName<<std::endl<< vprDEBUG_FLUSH;
    }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntity* ModelCADHandler::GetPart(std::string partID)
{
    std::map< std::string, VE_SceneGraph::CADEntity* >::iterator iter;
    iter = m_partList.find( partID );

    if ( iter == m_partList.end() )
    {
        for ( iter = m_partList.begin(); iter != m_partList.end(); ++iter )
        {
            std::cout << "Parts that are available: " << iter->first 
                        << " " << iter->second->GetFilename() << std::endl;
        }
        return 0;
    }

    return m_partList[ partID ];
}
/////////////////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* ModelCADHandler::GetAssembly(std::string assemblyID)
{
    return m_assemblyList[ assemblyID ];
}
/////////////////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::Clone* ModelCADHandler::GetClone(std::string cloneID)
{
    std::map< std::string, VE_SceneGraph::Clone* >::iterator iter;
    iter = m_cloneList.find( cloneID );

    if ( iter == m_cloneList.end() )
    {
        std::cout << "Clone not available: " << cloneID << std::endl;
        return 0;
    }
    return m_cloneList[ cloneID ];
}
/////////////////////////////////////////////////////////////////////////////////////////////
bool ModelCADHandler::PartExists(std::string partID)
{
    std::map<std::string,VE_SceneGraph::CADEntity*>::iterator foundPart;
    foundPart = m_partList.find( partID );

    if( foundPart != m_partList.end() )
    {
        return true;
    }
    return false;
}
/////////////////////////////////////////////////////////////////////////////////////////////
bool ModelCADHandler::AssemblyExists(std::string assemblyID)
{
    std::map< std::string, VE_SceneGraph::DCS* >::iterator foundAssembly;
    foundAssembly = m_assemblyList.find (assemblyID) ;

    if( foundAssembly != m_assemblyList.end() )
    {
        return true;
    }
    return false;
}
/////////////////////////////////////////////////////////////////////////////////////////////
bool ModelCADHandler::CloneExists(std::string cloneID)
{
    std::map< std::string, VE_SceneGraph::Clone* >::iterator foundClone;
    foundClone = m_cloneList.find( cloneID );

    if( foundClone != m_cloneList.end() )
    {
        return true;
    }
    return false;
}