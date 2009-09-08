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
    CompoundDragger( TransformationType::SCALE_COMPOUND ),
    m_explodeDistance( 0.2 ),
    m_xScaleAxis( NULL ),
    m_yScaleAxis( NULL ),
    m_zScaleAxis( NULL ),
    m_scaleUniform( NULL )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
ScaleCompound::ScaleCompound(
    const ScaleCompound& scaleCompound, const osg::CopyOp& copyop )
    :
    CompoundDragger( scaleCompound, copyop ),
    m_explodeDistance( scaleCompound.m_explodeDistance ),
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
const char* ScaleCompound::className() const
{
    return "ScaleCompound";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* ScaleCompound::clone( const osg::CopyOp& copyop ) const
{
    return new ScaleCompound( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* ScaleCompound::cloneType() const
{
    return new ScaleCompound();
}
////////////////////////////////////////////////////////////////////////////////
bool ScaleCompound::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const ScaleCompound* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleCompound::ComboForm()
{
    if( m_comboForm )
    {
        return;
    }

    //Call base method
    Dragger::ComboForm();

    osg::Vec3d explodeVector;
    osg::Vec3dArray* lineVertices( NULL );
    osg::Box* box( NULL );
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        ScaleAxis* scaleAxis = dynamic_cast< ScaleAxis* >( GetChild( i ) );
        if( scaleAxis )
        {
            //Get the explode vector
            explodeVector = scaleAxis->GetUnitAxis() * m_explodeDistance;

            //Move the lines and cylinders in from the origin and unit axis
            lineVertices = scaleAxis->GetLineVertices();
            (*lineVertices)[ 0 ] += explodeVector;
            (*lineVertices)[ 1 ] -= explodeVector;

            //Move the boxes in from the unit axis
            box = scaleAxis->GetBox();
            box->setCenter( box->getCenter() - explodeVector );
            
            //Update the geometry's display list
            scaleAxis->DirtyGeometry();
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
    Dragger::DefaultForm();

    osg::Vec3d explodeVector;
    osg::Vec3dArray* lineVertices( NULL );
    osg::Box* box( NULL );
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        ScaleAxis* scaleAxis = dynamic_cast< ScaleAxis* >( GetChild( i ) );
        if( scaleAxis )
        {
            //Get the explode vector
            explodeVector = scaleAxis->GetUnitAxis() * m_explodeDistance;

            //Move the lines and cylinders back to the origin and unit axis
            lineVertices = scaleAxis->GetLineVertices();
            (*lineVertices)[ 0 ] -= explodeVector;
            (*lineVertices)[ 1 ] += explodeVector;

            //Move the boxes back to the unit axis
            box = scaleAxis->GetBox();
            box->setCenter( box->getCenter() + explodeVector );

            //Update the geometry's display list
            scaleAxis->DirtyGeometry();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ScaleCompound::SetupDefaultGeometry()
{
    //Create translate z-axis dragger
    m_zScaleAxis = new ScaleAxis();
    m_zScaleAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );

    addChild( m_zScaleAxis.get() );

    //Create translate y-axis dragger
    m_yScaleAxis = new ScaleAxis();
    m_yScaleAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );

    //Rotate y-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 0.0, 1.0, 0.0 ) );
        m_yScaleAxis->setRotation( rotation );
    }

    addChild( m_yScaleAxis.get() );

    //Create translate x-axis dragger
    m_xScaleAxis = new ScaleAxis();
    m_xScaleAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );

    //Rotate x-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 1.0, 0.0, 0.0 ) );
        m_xScaleAxis->setRotation( rotation );
    }

    addChild( m_xScaleAxis.get() );

    //Create rotate twist dragger
    m_scaleUniform = new ScaleUniform();
    m_scaleUniform->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 1.0, 1.0, 1.0 ), true );

    addChild( m_scaleUniform.get() );
}
////////////////////////////////////////////////////////////////////////////////
