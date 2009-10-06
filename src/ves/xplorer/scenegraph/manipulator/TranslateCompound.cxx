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
#include <ves/xplorer/scenegraph/manipulator/TranslateCompound.h>
#include <ves/xplorer/scenegraph/manipulator/TranslateAxis.h>
#include <ves/xplorer/scenegraph/manipulator/TranslatePlane.h>
#include <ves/xplorer/scenegraph/manipulator/TranslatePan.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/ShapeDrawable>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TranslateCompound::TranslateCompound()
    :
    CompoundDragger( TransformationType::TRANSLATE_COMPOUND ),
    m_explodeDistance( 0.5 ),
    m_xTranslateAxis( NULL ),
    m_yTranslateAxis( NULL ),
    m_zTranslateAxis( NULL ),
    m_yzTranslatePlane( NULL ),
    m_xzTranslatePlane( NULL ),
    m_xyTranslatePlane( NULL ),
    m_translatePan( NULL )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
TranslateCompound::TranslateCompound(
    const TranslateCompound& translateCompound, const osg::CopyOp& copyop )
    :
    CompoundDragger( translateCompound, copyop ),
    m_explodeDistance( translateCompound.m_explodeDistance ),
    m_xTranslateAxis( translateCompound.m_xTranslateAxis.get() ),
    m_yTranslateAxis( translateCompound.m_yTranslateAxis.get() ),
    m_zTranslateAxis( translateCompound.m_zTranslateAxis.get() ),
    m_yzTranslatePlane( translateCompound.m_yzTranslatePlane.get() ),
    m_xzTranslatePlane( translateCompound.m_xzTranslatePlane.get() ),
    m_xyTranslatePlane( translateCompound.m_xyTranslatePlane.get() ),
    m_translatePan( translateCompound.m_translatePan.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TranslateCompound::~TranslateCompound()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
const char* TranslateCompound::className() const
{
    return "TranslateCompound";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslateCompound::clone( const osg::CopyOp& copyop ) const
{
    return new TranslateCompound( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslateCompound::cloneType() const
{
    return new TranslateCompound();
}
////////////////////////////////////////////////////////////////////////////////
bool TranslateCompound::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const TranslateCompound* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void TranslateCompound::ComboForm()
{
    if( m_comboForm )
    {
        return;
    }

    //Call base method
    Dragger::ComboForm();

    osg::Vec3d explodeVector;
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        TranslateAxis* translateAxis =
            dynamic_cast< TranslateAxis* >( GetChild( i ) );
        if( translateAxis )
        {
            //Turn off line and cylinder geometry
            translateAxis->EnableLinesAndCylinders( false );

            //Get the explode vector
            explodeVector = translateAxis->GetUnitAxis() * m_explodeDistance;

            //Move the cones out from the unit axis
            translateAxis->ConeCenterOffset( explodeVector );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void TranslateCompound::DefaultForm()
{
    if( !m_comboForm )
    {
        return;
    }

    //Call base method
    Dragger::DefaultForm();

    osg::Vec3d explodeVector;
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        TranslateAxis* translateAxis =
            dynamic_cast< TranslateAxis* >( GetChild( i ) );
        if( translateAxis )
        {
            //Turn on line and cylinder geometry
            translateAxis->EnableLinesAndCylinders( true );

            //Get the explode vector
            explodeVector = translateAxis->GetUnitAxis() * m_explodeDistance;

            //Move the cones back to the unit axis
            translateAxis->ConeCenterOffset( -explodeVector );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void TranslateCompound::SetupDefaultGeometry()
{
    //Create translate z-axis dragger
    m_zTranslateAxis = new TranslateAxis();
    m_zTranslateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );
    m_zTranslateAxis->ComboForm();

    addChild( m_zTranslateAxis.get() );

    //Create translate y-axis dragger
    m_yTranslateAxis = new TranslateAxis();
    m_yTranslateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );
    m_yTranslateAxis->ComboForm();

    //Rotate y-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 0.0, 1.0, 0.0 ) );
        m_yTranslateAxis->SetRotation( rotation );
    }

    addChild( m_yTranslateAxis.get() );

    //Create translate x-axis dragger
    m_xTranslateAxis = new TranslateAxis();
    m_xTranslateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );
    m_xTranslateAxis->ComboForm();

    //Rotate x-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 1.0, 0.0, 0.0 ) );
        m_xTranslateAxis->SetRotation( rotation );
    }

    addChild( m_xTranslateAxis.get() );

    //Create translate xy-plane dragger
    m_xyTranslatePlane = new TranslatePlane();
    m_xyTranslatePlane->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );
    m_xyTranslatePlane->ComboForm();

    addChild( m_xyTranslatePlane.get() );

    //Create translate xz-plane dragger
    m_xzTranslatePlane = new TranslatePlane();
    m_xzTranslatePlane->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );
    m_xzTranslatePlane->ComboForm();

    //Rotate xz-plane dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 0.0, -1.0, 0.0 ) );
        m_xzTranslatePlane->SetRotation( rotation );
    }

    addChild( m_xzTranslatePlane.get() );

    //Create translate yz-plane dragger
    m_yzTranslatePlane = new TranslatePlane();
    m_yzTranslatePlane->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );
    m_yzTranslatePlane->ComboForm();

    //Rotate yz-plane dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( -1.0, 0.0, 0.0 ) );
        m_yzTranslatePlane->SetRotation( -rotation );
    }

    addChild( m_yzTranslatePlane.get() );

    //Create translate pan dragger
    m_translatePan = new TranslatePan();
    m_translatePan->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 1.0, 1.0, 1.0 ), true );

    addChild( m_translatePan.get() );
}
////////////////////////////////////////////////////////////////////////////////
