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
#include <osg/Geometry>
#include <osg/LineWidth>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace bots;

const double PI = 3.14159265358979323846;

////////////////////////////////////////////////////////////////////////////////
BlockSensor::BlockSensor( bots::AgentEntity* agentEntity )
:
Sensor( agentEntity ),
mBlockInView( false ),
mCloseToBlock( false ),
mAngle( 0 ), 
mAngleInc( 0.1 ),
mRange( 0 ),
mNormalizedBlockVector( 0, 0, 0 ),
mLine( new osg::Geometry() ),
mBeamGeode( new osg::Geode() ),
mBeamLineSegment( new osg::LineSegment() )
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
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    colors->push_back( osg::Vec4( 0.0f, 1.0f, 1.0f, 1.0f ) );
    mLine->setColorArray( colors.get() );
    mLine->setColorBinding( osg::Geometry::BIND_OVERALL );

    lineNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    mLine->setNormalArray( lineNormals.get() );
    mLine->setNormalBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 1.0f );
    stateset->setAttribute( lineWidth.get() );
    mLine->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::CollectInformation()
{
    //Get the DCSs
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = mAgentEntity->GetPluginDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > agentDCS = mAgentEntity->GetDCS();
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > targetDCS = mAgentEntity->GetTargetDCS();

    osg::Vec3d startPoint, endPoint;
    double* agentPosition = agentDCS->GetVETranslationArray();
    startPoint.set( agentPosition[ 0 ],
                    agentPosition[ 1 ],
                    agentPosition[ 2 ] );
    if( targetDCS.valid() )
    {
        double* targetPosition = targetDCS->GetVETranslationArray();
        endPoint.set( targetPosition[ 0 ],
                      targetPosition[ 1 ],
                      0.5 );
    }
    else
    {
        Rotate();
        endPoint.set( agentPosition[ 0 ] + mRange * cos( mAngle ),
                      agentPosition[ 1 ] + mRange * sin( mAngle ),
                      agentPosition[ 2 ] );
    }

    //Reset results from last frame
    mBlockInView = false;
    mCloseToBlock = false;
    targetDCS = NULL;

    mBeamLineSegment->set( startPoint, endPoint );
    DrawLine( startPoint, endPoint );

    osgUtil::IntersectVisitor intersectVisitor;
    intersectVisitor.addLineSegment( mBeamLineSegment.get() );
    pluginDCS->accept( intersectVisitor );

    osgUtil::IntersectVisitor::HitList hitList = intersectVisitor.getHitList( mBeamLineSegment.get() );
    if( hitList.size() > 1 )
    {
        //Get the next hit excluding the agent itself
        osgUtil::Hit firstHit = hitList.at( 1 );

        osg::ref_ptr< osg::Geode > geode = firstHit.getGeode();

        if( geode.valid() )
        {
            osg::ref_ptr< osg::Vec4Array > colorArray = static_cast< osg::Vec4Array* >
                ( geode->getDrawable( 0 )->asGeometry()->getColorArray() );

            if( colorArray.valid() )
            {
                if( colorArray->at( 0 ).r() == 1.0 &&
                    colorArray->at( 0 ).g() == 1.0 &&
                    colorArray->at( 0 ).b() == 1.0 )
                {
                    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( geode.get() );
                    osg::ref_ptr< ves::xplorer::scenegraph::DCS > temp  = static_cast< ves::xplorer::scenegraph::DCS* >( parentVisitor.GetParentNode() );
                    if( temp.valid() )
                    {
                        double* blockPosition = temp->GetVETranslationArray();
                        btVector3 blockVector( blockPosition[ 0 ] - agentPosition[ 0 ],
                                               blockPosition[ 1 ] - agentPosition[ 1 ],
                                               0 );

                        if( blockVector.length() < 1.415 )//sqrt( 2 * 0.5 )
                        {
                            mCloseToBlock = true;
                        }

                        mNormalizedBlockVector = blockVector.normalize();

                        mBlockInView = true;
                        mAgentEntity->SetTargetDCS( temp.get() );
                    }
                }
            }
        }
    }

    
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::Rotate()
{
    mAngle += mAngleInc;
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = mAgentEntity->GetPluginDCS();

    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    mLine->setVertexArray( vertices.get() );

    mLine->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, vertices->size() ) );
    mBeamGeode->addDrawable( mLine.get() );      
    pluginDCS->addChild( mBeamGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::RemoveLine()
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > pluginDCS = mAgentEntity->GetPluginDCS();
    unsigned int numPrimitives = mLine->getNumPrimitiveSets();
    if( numPrimitives == 1 )
    {
        mLine->removePrimitiveSet( 0 );
    }
    mBeamGeode->removeDrawable( mLine.get() );
    pluginDCS->removeChild( mBeamGeode.get() );
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
btVector3 BlockSensor::GetNormalizedBlockVector()
{
    return mNormalizedBlockVector;
}
////////////////////////////////////////////////////////////////////////////////
void BlockSensor::SetRange( double range )
{
    mRange = range;
}
////////////////////////////////////////////////////////////////////////////////
