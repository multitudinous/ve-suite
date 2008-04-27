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
#include "BlockSensor.h"
#include "AgentEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
BlockSensor::BlockSensor( bots::AgentEntity* agentEntity )
    :
    Sensor( agentEntity ),
    mBlockInView( false ),
    mCloseToBlock( false ),
    mAngle( 0 ), 
    mAngleInc( 0.05 ),
    mRange( 0 ),
    mNormalizedBlockVector( 0, 0, 0 ),
    mGeode( 0 ),
    mGeometry( 0 ),
    mVertexArray( 0 ),
    mLineSegmentIntersector( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
BlockSensor::~BlockSensor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::Initialize()
{
    mLineSegmentIntersector = new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0, 0, 0 ), osg::Vec3( 0, 0, 0 ) );
    mGeode = new osg::Geode();
    mGeometry = new osg::Geometry();
    mVertexArray = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();

    //Only need 2 vertices for the line
    mVertexArray->resize( 2 );
    mGeometry->setVertexArray( mVertexArray.get() );

    colorArray->push_back( osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    mGeometry->setColorArray( colorArray.get() );
    mGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

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

    DisplayLine( false );
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::CollectInformation()
{
    //Get the DCSs
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS =
        mAgentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS =
        mAgentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS =
        mAgentEntity->GetTargetDCS();

    (*mVertexArray)[ 0 ] = agentDCS->getPosition();
    if( targetDCS.valid() )
    {
        (*mVertexArray)[ 1 ] = targetDCS->getPosition();
        (*mVertexArray)[ 1 ].z() = (*mVertexArray)[ 0 ].z();
    }
    else
    {
        Rotate();
        (*mVertexArray)[ 1 ] = (*mVertexArray)[ 0 ];
        (*mVertexArray)[ 1 ].x() += mRange * cos( mAngle );
        (*mVertexArray)[ 1 ].y() += mRange * sin( mAngle );
    }

    //Reset results from last frame
    mBlockInView = false;
    mCloseToBlock = false;
    targetDCS = NULL;
    mGeometry->dirtyDisplayList();
    mGeometry->dirtyBound();

    mLineSegmentIntersector->reset();
    mLineSegmentIntersector->setStart( (*mVertexArray)[ 0 ] );
    mLineSegmentIntersector->setEnd( (*mVertexArray)[ 1 ] );

    osgUtil::IntersectionVisitor intersectionVisitor(
        mLineSegmentIntersector.get() );
    pluginDCS->RemoveChild( agentDCS.get() );
    pluginDCS->accept( intersectionVisitor );
    pluginDCS->AddChild( agentDCS.get() );

    if( mLineSegmentIntersector->containsIntersections() )
    {
        osg::ref_ptr< osg::Drawable > drawable =
            mLineSegmentIntersector->getFirstIntersection().drawable;

        osg::Vec4 color = static_cast< osg::Vec4Array* >(
            drawable->asGeometry()->getColorArray() )->at( 0 );
        if( color.length() == 2.0 )
        {
            ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
                drawable->getParent( 0 ) );
            targetDCS = static_cast< ves::xplorer::scenegraph::DCS* >(
                parentVisitor.GetParentNode() );

            double* blockPosition = targetDCS->GetVETranslationArray();
            btVector3 blockVector(
                blockPosition[ 0 ] - (*mVertexArray)[ 0 ].x(),
                blockPosition[ 1 ] - (*mVertexArray)[ 0 ].y(), 0.0 );

            if( blockVector.length() < 1.415 )//sqrt( 2 * 0.5 )
            {
                mCloseToBlock = true;
            }

            mNormalizedBlockVector = blockVector.normalize();

            mBlockInView = true;
        }
    }

    mAgentEntity->SetTargetDCS( targetDCS.get() );

    //DisplayLine( true );
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::Rotate()
{
    mAngle += mAngleInc;
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::DisplayLine( bool onOff )
{
    mGeode->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
bool BlockSensor::BlockInView()
{
    return mBlockInView;
}
////////////////////////////////////////////////////////////////////////////////
bool BlockSensor::CloseToBlock()
{
    return mCloseToBlock;
}
////////////////////////////////////////////////////////////////////////////////
const btVector3& BlockSensor::GetNormalizedBlockVector()
{
    return mNormalizedBlockVector;
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::SetRange( double range )
{
    mRange = range;
}
////////////////////////////////////////////////////////////////////////////////
