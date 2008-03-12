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
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/ModelHandler.h>

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/util/Attribute.h>
#include <ves/xplorer/scenegraph/Clone.h>
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <ves/xplorer/scenegraph/util/OpacityVisitor.h>

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAttribute.h>
#include <ves/open/xml/Command.h>

#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/Socket/SocketAcceptor.h>
#include <vpr/System.h>

#include <osg/BlendFunc>
#include <osg/ClipPlane>

namespace ves
{
namespace xplorer
{
ModelCADHandler::ModelCADHandler( ves::xplorer::scenegraph::DCS* rootNode )
{
    m_assemblyList["rootNode"] = rootNode;
    //m_clipPlane = new osg::ClipPlane();
    //m_clipPlane->setClipPlane(1,0,0,-.5);
    /*m_assemblyList["rootNode"]
    ->getOrCreateStateSet()->setAttributeAndModes(m_clipPlane.get(),
                                                                  osg::StateAttribute::OFF);
    */
}
/////////////////////////////////////////////////////////////////////////////////////////////
ModelCADHandler::ModelCADHandler( const ModelCADHandler& rhs )
{
    m_cloneList = rhs.m_cloneList;
    m_partList = rhs.m_partList;
    m_assemblyList = rhs.m_assemblyList;
    m_rootCADNodeID = rhs.m_rootCADNodeID;
    m_nodeAttributes = rhs.m_nodeAttributes;
    m_globalAttributeList = rhs.m_globalAttributeList;
    m_clipPlane = new osg::ClipPlane( *rhs.m_clipPlane.get() );
}
/////////////////////////////////////////////////////////////////////////////////////////////
ModelCADHandler::~ModelCADHandler()
{
    std::map<std::string, ves::xplorer::scenegraph::CADEntity*>::iterator iter;
    for( iter = m_partList.begin(); iter != m_partList.end(); iter++ )
    {
        ModelHandler::instance()->UnregisterCADFile( iter->second );
        delete iter->second;
    }
    m_partList.clear();

    ///Assembly list is map of dcs's which are osg smart pointers
    m_assemblyList.clear();

    for( std::map < std::string,
            ves::xplorer::scenegraph::Clone* >::iterator itr = m_cloneList.begin();
            itr != m_cloneList.end(); itr++ )
    {
        delete itr->second;
    }
    m_cloneList.clear();
    m_nodeAttributes.clear();
    m_globalAttributeList.clear();
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
        m_globalAttributeList = rhs.m_globalAttributeList;
    }
    return *this;
}
/////////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::SetClipPlane( double a, double b, double c, double d )
{
    if( m_clipPlane.valid() )
    {
        m_clipPlane->setClipPlane( a, b, c, d );
    }
}
//////////////////////////////////////////////////////////////////
void ModelCADHandler::ToggleClipPlane( bool onOff )
{
    if( !m_clipPlane.valid() )
    {
        return;
    }
    if( onOff )
    {
        m_assemblyList["rootNode"]->getOrCreateStateSet()
        ->setAssociatedModes( m_clipPlane.get(), osg::StateAttribute::ON );
    }
    else
    {
        m_assemblyList["rootNode"]->getOrCreateStateSet()
        ->setAssociatedModes( m_clipPlane.get(), osg::StateAttribute::OFF );
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::SetRootCADNodeID( std::string rootNodeId )
{
    m_rootCADNodeID = rootNodeId;
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::CreateClone( std::string cloneID,
                                   std::string originalID,
                                   std::string originalType )
{
    if( originalType == std::string( "Assembly" ) )
    {

        if( GetAssembly( originalID ) )
        {
            m_cloneList[ cloneID ] =
                new ves::xplorer::scenegraph::Clone( GetAssembly( originalID ) );
        }
    }
    else if( originalType == std::string( "Part" ) )
    {
        if( GetPart( originalID ) )
        {
            m_cloneList[ cloneID ] =
                new ves::xplorer::scenegraph::Clone( GetPart( originalID )->GetNode()->GetNode() );
        }
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::CreateAssembly( std::string assemblyID )
{
    m_assemblyList[ assemblyID ] = new ves::xplorer::scenegraph::DCS();
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::CreatePart( std::string fileName, std::string partID,
                                  std::string parentID )
{
    ves::xplorer::scenegraph::CADEntity* tempCAD =
        ModelHandler::instance()->IsCADFileLoaded( fileName );
    if( tempCAD )
    {
        ///If we have already loaded the parts
        ves::xplorer::scenegraph::CADEntityHelper* tempNode = tempCAD->GetNode();
        m_partList[ partID ] =
            new ves::xplorer::scenegraph::CADEntity(
                tempNode,
                m_assemblyList[ parentID ].get(),
                ves::xplorer::scenegraph::PhysicsSimulator::instance() );
        vprDEBUG( vesDBG, 1 ) << "|\t--Cloned new part--"
        << std::endl << vprDEBUG_FLUSH;
    }
    else
    {
        ///If we have not loaded this part
        ///turn on occlusion culling by default
        m_partList[ partID ] =
            new ves::xplorer::scenegraph::CADEntity(
                fileName,
                m_assemblyList[ parentID ].get(),
                false,
                true,
                ves::xplorer::scenegraph::PhysicsSimulator::instance() );
        vprDEBUG( vesDBG, 1 ) << "|\t--Loaded new part--"
        << std::endl << vprDEBUG_FLUSH;
    }
    ModelHandler::instance()->RegisterCADFile( m_partList[ partID ] );
    //add key pointer to physics map for bullet rigid body
    //add data pair for transform node
}
/////////////////////////////////////////////////////
void ModelCADHandler::RemoveNode( std::string nodeID,
                                  std::string nodeType )
{
    //first remove all the attributes for this node
    std::map < std::string,
    std::vector < std::pair < std::string,
    osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find( nodeID );
    if( attributeList != m_nodeAttributes.end() )
    {
        //clear out this attribute list associated with this node
        m_nodeAttributes.erase( attributeList );
    }
    //remove the node from the list
    if( nodeType == "Assembly" )
    {
        m_assemblyList.erase( nodeID );
    }
    else if( nodeType == "Part" )
    {
        m_partList.erase( nodeID );
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::SetActiveAttributeOnNode( std::string nodeID,
                                                std::string nodeType,
                                                std::string attributeName )
{
    std::map < std::string,
    std::vector < std::pair < std::string,
    osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find( nodeID );

    if( attributeList != m_nodeAttributes.end() )
    {

        std::vector < std::pair < std::string,
        osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
        std::vector < std::pair < std::string,
        osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
        namesAndAttributes = attributeList->second;
        for( foundAttribute = namesAndAttributes.begin();
                foundAttribute != namesAndAttributes.end();
                foundAttribute++ )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tFound attribute: "
            << foundAttribute->first
            << std::endl << vprDEBUG_FLUSH;
            if( foundAttribute->first == attributeName )
            {
                if( nodeType == "Assembly" )
                {
                    vprDEBUG( vesDBG, 1 ) << "|\tSetting Assembly attribute: "
                    << foundAttribute->first
                    << std::endl << vprDEBUG_FLUSH;
                    GetAssembly( nodeID )->setStateSet( foundAttribute->second.get() );
                    return;
                }
                else if( nodeType == "Part" )
                {
                    vprDEBUG( vesDBG, 1 ) << "|\tSetting Part attribute: "
                    << foundAttribute->first
                    << std::endl << vprDEBUG_FLUSH;
                    GetPart( nodeID )->GetDCS()->setStateSet( foundAttribute->second.get() );
                    vprDEBUG( vesDBG, 1 ) << "|\tvalid: "
                    << foundAttribute->first << std::endl
                    << vprDEBUG_FLUSH;
                    return;
                }
                else if( nodeType == "Clone" )
                {
                    vprDEBUG( vesDBG, 1 ) << "|\tSetting Clone attribute: "
                    << foundAttribute->first
                    << std::endl
                    << vprDEBUG_FLUSH;
                    GetClone( nodeID )->
                    GetClonedGraph()->setStateSet( foundAttribute->second.get() );
                    return;
                }
            }
        }
    }
    else
    {
        vprDEBUG( vesDBG, 1 ) << "|\tAttribute not found on node: "
        << attributeName << std::endl
        << vprDEBUG_FLUSH;
    }
}
////////////////////////////////////////////////////////////////////////
void ModelCADHandler::UpdateOpacity( std::string nodeID, float opacity )
{
    try
    {
        bool transparent = true;
        if( opacity == 1.f )
        {
            transparent = false;
        }
        if( AssemblyExists(nodeID) )
        {
            ves::xplorer::scenegraph::util::OpacityVisitor
            opacity_visitor( m_assemblyList[nodeID].get(), transparent, opacity );
        }
        else if( PartExists( nodeID ) )
        {
            ves::xplorer::scenegraph::util::OpacityVisitor
            opacity_visitor( m_partList[nodeID]->GetDCS(), transparent, opacity );
        }
    }
    catch ( ... )
    {
        vprDEBUG( vesDBG, 1 ) << "|\t CADNode not found!!!"
            << std::endl
            << vprDEBUG_FLUSH;
        vprDEBUG( vesDBG, 1 ) << "|\tModelCADHandler::UpdateOpacity()---"
            << std::endl << vprDEBUG_FLUSH;
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::MakeCADRootTransparent()
{
    if( !AssemblyExists( m_rootCADNodeID ) )
    {
        return;
    }

    // Put in logic to deal with transparency on a per component basis
    for( std::map< std::string, ves::xplorer::scenegraph::CADEntity* >::iterator 
        iter = m_partList.begin(); iter != m_partList.end(); ++iter )
    {
        if( iter->second->GetTransparentFlag() )
        {
            ves::xplorer::scenegraph::util::OpacityVisitor
            opacity_visitor( iter->second->GetDCS(), true, 0.3f );
        }
    }
 }
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::MakeCADRootOpaque()
{
    if( !AssemblyExists( m_rootCADNodeID ) )
    {
        return;
    }

    // Put in logic to deal with transparency on a per component basis
    for( std::map< std::string, ves::xplorer::scenegraph::CADEntity* >::iterator 
        iter = m_partList.begin(); iter != m_partList.end(); ++iter )
    {
        if( iter->second->GetTransparentFlag() )
        {
            ves::xplorer::scenegraph::util::OpacityVisitor
            opacity_visitor( iter->second->GetDCS(), false, 1.0f );
        }
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::RemoveAttributeFromNode( std::string nodeID,
                                               std::string nodeType,
                                               std::string attributeName )
{
#ifdef _OSG
    std::map < std::string,
    std::vector < std::pair < std::string,
    osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find( nodeID );


    if( attributeList != m_nodeAttributes.end() )
    {
        std::vector< std::pair<std::string, osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
        std::vector< std::pair<std::string, osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
        namesAndAttributes = attributeList->second;
        for( foundAttribute = namesAndAttributes.begin();
                foundAttribute != namesAndAttributes.end(); )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tFound attribute: "
            << foundAttribute->first
            << std::endl << vprDEBUG_FLUSH;
            if( foundAttribute->first == attributeName )
            {
                namesAndAttributes.erase( foundAttribute );

                if( nodeType == "Assembly" )
                {
                    GetAssembly( nodeID )->getStateSet()->clear();
                }
                else if( nodeType == "Part" )
                {
                    GetPart( nodeID )->GetDCS()->getStateSet()->clear();
                }
                else if( nodeType == "Clone" )
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
        vprDEBUG( vesDBG, 1 ) << "|\tAttribute not found: "
        << attributeName << std::endl
        << vprDEBUG_FLUSH;
    }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::AddAttributeToNode( std::string nodeID,
                                          ves::open::xml::cad::CADAttributePtr
                                          newAttribute )
{
#ifdef _OSG
    vprDEBUG( vesDBG, 1 ) << "|\tModelCADHandler::AddAttributeToNode()---" << std::endl << vprDEBUG_FLUSH;
    osg::ref_ptr<ves::xplorer::scenegraph::util::Attribute> attribute;
    std::map<std::string, osg::ref_ptr<osg::StateSet> >::iterator
    itr = m_globalAttributeList.find( newAttribute->GetAttributeName() ) ;
    if( itr != m_globalAttributeList.end() )
    {
        attribute = dynamic_cast<ves::xplorer::scenegraph::util::Attribute*>(( *itr ).second.get() );
    }
    else
    {
        attribute = new ves::xplorer::scenegraph::util::Attribute();
        attribute->CreateStateSetFromAttribute( newAttribute );
        m_globalAttributeList[newAttribute->GetAttributeName()] = attribute.get();
    }
    std::pair<std::string, osg::ref_ptr< osg::StateSet > > attributeInfo;
    attributeInfo.first = newAttribute->GetAttributeName();
    attributeInfo.second = attribute.get();

    std::map< std::string, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find( nodeID );

    if( attributeList != m_nodeAttributes.end() )
    {
        vprDEBUG( vesDBG, 1 ) << "|\tAdding attribute: " << attributeList->first << std::endl << vprDEBUG_FLUSH;
        attributeList->second.push_back( attributeInfo );
    }
    else
    {
        std::vector < std::pair < std::string,
        osg::ref_ptr< osg::StateSet > > > temp;
        temp.push_back( attributeInfo );
        m_nodeAttributes[ nodeID ] = temp;

        //create the empty state set to restore defaults
        std::pair<std::string, osg::ref_ptr< osg::StateSet > > defaultAttributeInfo;
        defaultAttributeInfo.first = "Default Attribute";
        defaultAttributeInfo.second = new osg::StateSet();
        m_nodeAttributes[ nodeID ].push_back( defaultAttributeInfo );
    }
    vprDEBUG( vesDBG, 1 ) << "|\tend ModelCADHandler::AddAttributeToNode()---"
    << std::endl << vprDEBUG_FLUSH;
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::UpdateMaterialMode( std::string nodeID,
                                          std::string attributeName,
                                          std::string type,
                                          std::string mode )
{
#ifdef _OSG
    std::map< std::string, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find( nodeID );

    if( attributeList != m_nodeAttributes.end() )
    {
        std::vector< std::pair<std::string, osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
        std::vector< std::pair<std::string, osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
        namesAndAttributes = attributeList->second;
        for( foundAttribute = namesAndAttributes.begin();
                foundAttribute != namesAndAttributes.end();
                foundAttribute++ )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tFound attribute: " << foundAttribute->first << std::endl << vprDEBUG_FLUSH;
            if( foundAttribute->first == attributeName )
            {
                ///update the material component
                osg::ref_ptr<ves::xplorer::scenegraph::util::Attribute> attribute =
                    dynamic_cast<ves::xplorer::scenegraph::util::Attribute*>( foundAttribute->second.get() );
                if( attribute.valid() )
                {
                    attribute->UpdateMaterialMode( type, mode );
                }
                else
                {
                    vprDEBUG( vesDBG, 1 ) << "|\tAttribute not found: " << attributeName << std::endl << vprDEBUG_FLUSH;
                }
            }
        }
    }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
void ModelCADHandler::UpdateMaterialComponent( std::string nodeID,
                                               std::string attributeName,
                                               std::string component,
                                               std::string face,
                                               std::vector<double> values )
{
#ifdef _OSG
    std::map < std::string,
    std::vector < std::pair < std::string,
    osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
    attributeList = m_nodeAttributes.find( nodeID );

    if( attributeList != m_nodeAttributes.end() )
    {
        std::vector< std::pair<std::string, osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
        std::vector< std::pair<std::string, osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
        namesAndAttributes = attributeList->second;
        for( foundAttribute = namesAndAttributes.begin();
                foundAttribute != namesAndAttributes.end();
                foundAttribute++ )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tFound attribute: "
            << foundAttribute->first
            << std::endl << vprDEBUG_FLUSH;
            if( foundAttribute->first == attributeName )
            {
                ///update the material component
                osg::ref_ptr<ves::xplorer::scenegraph::util::Attribute> attribute =
                    dynamic_cast<ves::xplorer::scenegraph::util::Attribute*>
                    ( foundAttribute->second.get() );
                if( attribute.valid() )
                {
                    attribute->UpdateMaterial( component, face, values );
                }
                else
                {
                    vprDEBUG( vesDBG, 1 ) << "|\tAttribute not found: " << attributeName << std::endl << vprDEBUG_FLUSH;
                }
            }
        }
    }
    else
    {
        vprDEBUG( vesDBG, 1 ) << "|\tAttribute not found: " << attributeName << std::endl << vprDEBUG_FLUSH;
    }
#endif
}
/////////////////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::CADEntity* ModelCADHandler::GetPart( std::string partID )
{
    std::map< std::string, ves::xplorer::scenegraph::CADEntity* >::iterator iter;
    iter = m_partList.find( partID );

    if( iter == m_partList.end() )
    {
        for( iter = m_partList.begin(); iter != m_partList.end(); ++iter )
        {
            std::cout << "Parts that are available: " << iter->first
            << " " << iter->second->GetFilename() << std::endl;
        }
        return 0;
    }

    return m_partList[ partID ];
}
/////////////////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* ModelCADHandler::GetAssembly( std::string assemblyID )
{
    return m_assemblyList[ assemblyID ].get();
}
/////////////////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Clone* ModelCADHandler::GetClone( std::string cloneID )
{
    std::map< std::string, ves::xplorer::scenegraph::Clone* >::iterator iter;
    iter = m_cloneList.find( cloneID );

    if( iter == m_cloneList.end() )
    {
        std::cout << "Clone not available: " << cloneID << std::endl;
        return 0;
    }
    return m_cloneList[ cloneID ];
}
/////////////////////////////////////////////////////////////////////////////////////////////
bool ModelCADHandler::PartExists( std::string partID )
{
    std::map<std::string, ves::xplorer::scenegraph::CADEntity*>::iterator foundPart;
    foundPart = m_partList.find( partID );

    if( foundPart != m_partList.end() )
    {
        return true;
    }
    return false;
}
/////////////////////////////////////////////////////////////////////////////////////////////
bool ModelCADHandler::AssemblyExists( std::string assemblyID )
{
    std::map< std::string, osg::ref_ptr< ves::xplorer::scenegraph::DCS > >::iterator
    foundAssembly;
    foundAssembly = m_assemblyList.find( assemblyID ) ;

    if( foundAssembly != m_assemblyList.end() )
    {
        return true;
    }
    return false;
}
/////////////////////////////////////////////////////////////////////////////////////////////
bool ModelCADHandler::CloneExists( std::string cloneID )
{
    std::map< std::string, ves::xplorer::scenegraph::Clone* >::iterator foundClone;
    foundClone = m_cloneList.find( cloneID );

    if( foundClone != m_cloneList.end() )
    {
        return true;
    }
    return false;
}

} // end xplorer
} // end ves
