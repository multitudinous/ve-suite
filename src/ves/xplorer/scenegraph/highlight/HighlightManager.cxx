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

// --- VE-Suite Includes --- //
#include <ves/xplorer/communication/CommunicationHandler.h>

#include <ves/xplorer/scenegraph/highlight/HighlightManager.h>
#include <ves/xplorer/scenegraph/highlight/CircleHighlight.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
//#include <ves/xplorer/scenegraph/HeadPositionCallback.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/Transform.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Depth>
#include <osg/LineWidth>

#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

#include <boost/lexical_cast.hpp>

using namespace ves::xplorer::scenegraph::highlight;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
HighlightManager::HighlightManager()
    :
    osg::Group(),
    m_enabled( false ),
    m_toggled( false ),
    m_activeCircleHighlight( NULL )
{
    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();
    stateSet->setRenderBinDetails( 11, std::string( "DepthSortedBin" ) );

    setCullingActive( false );

    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    depth->setFunction( osg::Depth::ALWAYS );
    depth->setWriteMask( false );
    stateSet->setAttributeAndModes(
        depth.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    Enable();
}
////////////////////////////////////////////////////////////////////////////////
HighlightManager::HighlightManager(
    const HighlightManager& highlightManager, const osg::CopyOp& copyop )
    :
    osg::Group( highlightManager, copyop ),
    m_enabled( highlightManager.m_enabled ),
    m_activeCircleHighlight( highlightManager.m_activeCircleHighlight ),
    m_tagNames( highlightManager.m_tagNames )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
HighlightManager::~HighlightManager()
{
    m_tagNames.clear();
    m_nodeToCircleHighlights.clear();
}
////////////////////////////////////////////////////////////////////////////////
bool HighlightManager::addChild( CircleHighlight* child )
{
    SetActiveCircleHighlight( child );

    if( child )
    {
        UpdateConductorData();
    }

    return osg::Group::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
CircleHighlight* const HighlightManager::ConvertNodeToCircleHighlight(
    osg::Node* const node )
{
    return static_cast< CircleHighlight* >( node );
}
////////////////////////////////////////////////////////////////////////////////
void HighlightManager::Enable( const bool& enable )
{
    m_enabled = enable;

    if( m_enabled )
    {
        setNodeMask( 1 );
        //setNodeMask( NodeMask::HIGHLIGHT_MANAGER );
    }
    else
    {
        setNodeMask( NodeMask::NONE );
    }
}
////////////////////////////////////////////////////////////////////////////////
CircleHighlight* const HighlightManager::GetActiveCircleHighlight() const
{
    return m_activeCircleHighlight;
}
////////////////////////////////////////////////////////////////////////////////
bool HighlightManager::insertChild( unsigned int index, CircleHighlight* child )
{
    return osg::Group::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
const bool HighlightManager::IsEnabled() const
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
bool const& HighlightManager::IsToggled() const
{
    return m_toggled;
}
////////////////////////////////////////////////////////////////////////////////
void HighlightManager::removeChildren()
{
    //SetActiveCircleHighlight( NULL );

    _children.clear();
    m_tagNames.clear();
    m_nodeToCircleHighlights.clear();
}
////////////////////////////////////////////////////////////////////////////////
bool HighlightManager::replaceChild(
    CircleHighlight* origChild, CircleHighlight* newChild )
{
    return osg::Group::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
void HighlightManager::SetActiveCircleHighlight(
    CircleHighlight* circleHighlight )
{
    if( circleHighlight == m_activeCircleHighlight )
    {
        return;
    }

    if( circleHighlight )
    {
        ;
    }
    else
    {
        ;
    }

    //Set the active highlight
    m_activeCircleHighlight = circleHighlight;
}
////////////////////////////////////////////////////////////////////////////////
bool HighlightManager::setChild( unsigned int i, CircleHighlight* node )
{
    return osg::Group::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
void HighlightManager::Toggle( bool const& toggle )
{
    m_toggled = toggle;
}
////////////////////////////////////////////////////////////////////////////////
void HighlightManager::UpdateConductorData()
{
    unsigned int position = getChildIndex( m_activeCircleHighlight );
    std::string name = m_activeCircleHighlight->getName();

    open::xml::CommandPtr command( new open::xml::Command() );
    command->SetCommandName( "UPDATE_NEW_MARKER_OBJECT" );

    open::xml::DataValuePairPtr dvpI( new open::xml::DataValuePair() );
    dvpI->SetData( "MarkerPosition", position );
    command->AddDataValuePair( dvpI );

    open::xml::DataValuePairPtr dvpII( new open::xml::DataValuePair() );
    dvpII->SetData( "MarkerName", name );
    command->AddDataValuePair( dvpII );

    communication::CommunicationHandler::instance()->SetXMLCommand( command );
}
////////////////////////////////////////////////////////////////////////////////
const std::string& HighlightManager::GetNextTagName()
{
    std::string tempName("Label ");
    size_t numNames = m_tagNames.size();
    tempName.append( boost::lexical_cast<std::string>( numNames ) );
    m_tagNames.push_back( tempName );
    return *(m_tagNames.end()-1);
}
////////////////////////////////////////////////////////////////////////////////
bool HighlightManager::IsNodeCircled( osg::Node* inNode )
{
    NodeToCircleHighlightsIter iter = m_nodeToCircleHighlights.find( inNode );
    if( iter != m_nodeToCircleHighlights.end() )
    {
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void HighlightManager::RegisterNodeAndHighlight( osg::Node* inNode, CircleHighlight* circle )
{
    if( IsNodeCircled( inNode ) )
    {
        return;
    }
    m_nodeToCircleHighlights[ inNode ] = circle;
}
