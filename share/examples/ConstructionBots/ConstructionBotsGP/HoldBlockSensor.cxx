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

// --- My Includes --- //
#include "HoldBlockSensor.h"
#include "AgentEntity.h"

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

#include <osgUtil/LineSegmentIntersector>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
HoldBlockSensor::HoldBlockSensor( bots::AgentEntity* agentEntity )
    :
    Sensor( agentEntity ),
    mHoldingBlock( false ),
    mRange( 0.6 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
HoldBlockSensor::~HoldBlockSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HoldBlockSensor::Initialize()
{
    mLineSegmentIntersector = new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 0, 0 ) );
    mGeode = new osg::Geode();
    mGeometry = new osg::Geometry();
    mVertexArray = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();

    mVertexArray->resize( 2 );
    mGeometry->setVertexArray( mVertexArray.get() );

    colorArray->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    mGeometry->setColorArray( colorArray.get() );
    mGeometry->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    mGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

    mGeode->addDrawable( mGeometry.get() );

    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 1.0f );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    stateset->setAttribute( lineWidth.get() );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    mGeode->setStateSet( stateset.get() );

    mAgentEntity->GetPluginDCS()->addChild( mGeode.get() );

    DisplayGeometry( false );
}
////////////////////////////////////////////////////////////////////////////////
void HoldBlockSensor::CollectInformation()
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS =
        mAgentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS =
        mAgentEntity->GetDCS();

    osg::Vec3d startPoint = agentDCS->getPosition();
    osg::Vec3d endPoint = startPoint;
    endPoint.z() += mRange;

    //Reset results from last frame
    mHoldingBlock = false;

    mLineSegmentIntersector->reset();
    mLineSegmentIntersector->setStart( startPoint );
    mLineSegmentIntersector->setEnd( endPoint );

    osgUtil::IntersectionVisitor intersectionVisitor(
        mLineSegmentIntersector.get() );
    pluginDCS->RemoveChild( agentDCS.get() );
    pluginDCS->accept( intersectionVisitor );
    pluginDCS->AddChild( agentDCS.get() );

    if( mLineSegmentIntersector->containsIntersections() )
    {
        mHoldingBlock = true;
    }

    if( !mHoldingBlock )
    {
        mAgentEntity->mHeldBlock = NULL;

        if( mAgentEntity->mBuildMode )
        {
            mAgentEntity->mBuildMode = false;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool HoldBlockSensor::HoldingBlock()
{
    return mHoldingBlock;
}
////////////////////////////////////////////////////////////////////////////////
