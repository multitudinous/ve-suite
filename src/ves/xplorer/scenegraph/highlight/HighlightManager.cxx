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
#include <ves/xplorer/scenegraph/highlight/HighlightManager.h>
#include <ves/xplorer/scenegraph/highlight/CircleHighlight.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
//#include <ves/xplorer/scenegraph/HeadPositionCallback.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Depth>
#include <osg/LineWidth>

#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph::highlight;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
HighlightManager::HighlightManager()
    :
    osg::Group(),
    m_enabled( false ),
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
    m_activeCircleHighlight( highlightManager.m_activeCircleHighlight )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
HighlightManager::~HighlightManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool HighlightManager::addChild( std::string const& name )
{
    osg::ref_ptr< CircleHighlight > circleHighlight = new CircleHighlight();
    /*
    DCS* worldDCS = SceneManager::instance()->GetWorldDCS();
    osg::ref_ptr< CircleHighlight > circleHighlight = new CircleHighlight();
    circleHighlight->setName( name );
    DCS& dcs = circleHighlight->GetDCS();
    gmtl::Matrix44d tempMat( worldDCS->GetMat() );
    dcs.SetMat( gmtl::invert( tempMat ) );
    */

    return osg::Group::addChild( circleHighlight.get() );
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
void HighlightManager::removeChildren()
{
    //SetActiveCircleHighlight( NULL );

    _children.clear();
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
    /*
    if( cameraObject == m_activeCircleHighlight )
    {
        return;
    }

    //Turn off rendering for previously active camera
    if( m_activeCircleHighlight )
    {
        m_activeCircleHighlight->EnableCamera( false );
    }

    if( cameraObject )
    {
        cameraObject->SetRenderQuadTexture( *(m_rttQuad.get()) );
        cameraObject->EnableCamera();
        m_rttQuadTransform->setNodeMask( 1 );
    }
    else
    {
        m_rttQuadTransform->setNodeMask( 0 );
    }

    //Set the active camera
    m_activeCircleHighlight = cameraObject;
    */
}
////////////////////////////////////////////////////////////////////////////////
bool HighlightManager::setChild( unsigned int i, CircleHighlight* node )
{
    return osg::Group::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
