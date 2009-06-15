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
#include <ves/xplorer/scenegraph/manipulator/ScaleCompound.h>
#include <ves/xplorer/scenegraph/manipulator/ScaleAxis.h>
#include <ves/xplorer/scenegraph/manipulator/ScaleUniform.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/ShapeDrawable>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ScaleCompound::ScaleCompound()
    :
    CompoundDragger(),
    m_boxExplodeVector( -0.2, 0.0, 0.0 ),
    m_xScaleAxis( NULL ),
    m_yScaleAxis( NULL ),
    m_zScaleAxis( NULL ),
    m_scaleUniform( NULL )
{
    m_transformationType = TransformationType::SCALE_COMPOUND;

    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
ScaleCompound::ScaleCompound(
    const ScaleCompound& scaleCompound, const osg::CopyOp& copyop )
    :
    CompoundDragger( scaleCompound, copyop ),
    m_boxExplodeVector( scaleCompound.m_boxExplodeVector ),
    m_xScaleAxis( scaleCompound.m_xScaleAxis.get() ),
    m_yScaleAxis( scaleCompound.m_yScaleAxis.get() ),
    m_zScaleAxis( scaleCompound.m_zScaleAxis.get() ),
    m_scaleUniform( scaleCompound.m_scaleUniform.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ScaleCompound::~ScaleCompound()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleCompound::ComboForm()
{
    if( m_comboForm )
    {
        return;
    }

    //Call base method
    CompoundDragger::ComboForm();

    osg::Vec3Array* lineVertices( NULL );
    osg::Geometry* lineGeometry( NULL );
    osg::Box* box( NULL );
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        ScaleAxis* scaleAxis = dynamic_cast< ScaleAxis* >( GetChild( i ) );
        if( scaleAxis )
        {
            //Move the lines and cylinders in from the origin and unit axis
            lineVertices = scaleAxis->GetLineVertices();
            (*lineVertices)[ 0 ] -= m_boxExplodeVector;
            (*lineVertices)[ 1 ] += m_boxExplodeVector;

            lineGeometry = scaleAxis->GetLineGeometry();
            lineGeometry->dirtyDisplayList();
            lineGeometry->dirtyBound();

            //Move the boxes in from the unit axis
            box = scaleAxis->GetBox();
            box->setCenter( box->getCenter() + m_boxExplodeVector );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ScaleCompound::DefaultForm()
{
    if( !m_comboForm )
    {
        return;
    }

    //Call base method
    CompoundDragger::DefaultForm();

    osg::Vec3Array* lineVertices( NULL );
    osg::Geometry* lineGeometry( NULL );
    osg::Box* box( NULL );
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        ScaleAxis* scaleAxis = dynamic_cast< ScaleAxis* >( GetChild( i ) );
        if( scaleAxis )
        {
            //Move the lines and cylinders back to the origin and unit axis
            lineVertices = scaleAxis->GetLineVertices();
            (*lineVertices)[ 0 ] += m_boxExplodeVector;
            (*lineVertices)[ 1 ] -= m_boxExplodeVector;

            lineGeometry = scaleAxis->GetLineGeometry();
            lineGeometry->dirtyDisplayList();
            lineGeometry->dirtyBound();

            //Move the boxes back to the unit axis
            box = scaleAxis->GetBox();
            box->setCenter( box->getCenter() - m_boxExplodeVector );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ScaleCompound::SetupDefaultGeometry()
{
    //Create translate x-axis dragger
    m_xScaleAxis = new ScaleAxis();
    m_xScaleAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );

    addChild( m_xScaleAxis.get() );

    //Create translate y-axis dragger
    m_yScaleAxis = new ScaleAxis();
    m_yScaleAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );

    //Rotate y-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate(
            osg::Vec3d( 1.0, 0.0, 0.0 ), osg::Vec3d( 0.0, 1.0, 0.0 ) );
        m_yScaleAxis->setMatrix( osg::Matrix( rotation ) );
    }

    addChild( m_yScaleAxis.get() );

    //Create translate z-axis dragger
    m_zScaleAxis = new ScaleAxis();
    m_zScaleAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );

    //Rotate z-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate(
            osg::Vec3d( 1.0, 0.0, 0.0 ), osg::Vec3d( 0.0, 0.0, 1.0 ) );
        m_zScaleAxis->setMatrix( osg::Matrix( rotation ) );
    }

    addChild( m_zScaleAxis.get() );

    //Create rotate twist dragger
    m_scaleUniform = new ScaleUniform();
    m_scaleUniform->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 0.0, 1.0, 1.0, 1.0 ), true );

    addChild( m_scaleUniform.get() );
}
////////////////////////////////////////////////////////////////////////////////
