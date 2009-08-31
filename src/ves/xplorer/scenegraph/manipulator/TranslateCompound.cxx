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
void TranslateCompound::accept( osg::NodeVisitor& nv )
{
    if( nv.validNodeMask( *this ) )
    {
        nv.pushOntoNodePath( this );
        nv.apply( *this );
        nv.popFromNodePath();
    }
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
const char* TranslateCompound::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
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
    osg::Geode* geode( NULL );
    osg::Cone* cone( NULL );
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        TranslateAxis* translateAxis =
            dynamic_cast< TranslateAxis* >( GetChild( i ) );
        if( translateAxis )
        {
            //Get the explode vector
            explodeVector = translateAxis->GetUnitAxis() * m_explodeDistance;

            //Turn off line and cylinder geometry
            geode = translateAxis->GetLineAndCylinderGeode();
            geode->setNodeMask( 0 );

            //Move the cones out from the unit axis
            cone = translateAxis->GetCone();
            cone->setCenter( cone->getCenter() + explodeVector );
            //Update the geometry's display list
            translateAxis->DirtyCone();
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
    osg::Geode* geode( NULL );
    osg::Cone* cone( NULL );
    for( unsigned int i = 0; i < getNumChildren(); ++i )
    {
        TranslateAxis* translateAxis =
            dynamic_cast< TranslateAxis* >( GetChild( i ) );
        if( translateAxis )
        {
            //Get the explode vector
            explodeVector = translateAxis->GetUnitAxis() * m_explodeDistance;

            //Turn on line and cylinder geometry
            geode = translateAxis->GetLineAndCylinderGeode();
            geode->setNodeMask( 1 );

            //Move the cones back to the unit axis
            cone = translateAxis->GetCone();
            cone->setCenter( cone->getCenter() - explodeVector );
            //Update the geometry's display list
            translateAxis->DirtyCone();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void TranslateCompound::SetupDefaultGeometry()
{
    //Create translate x-axis dragger
    m_xTranslateAxis = new TranslateAxis();
    m_xTranslateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );
    m_xTranslateAxis->ComboForm();

    addChild( m_xTranslateAxis.get() );

    //Create translate y-axis dragger
    m_yTranslateAxis = new TranslateAxis();
    m_yTranslateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );
    m_yTranslateAxis->ComboForm();

    //Rotate y-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 0.0, 1.0, 0.0 ) );
        m_yTranslateAxis->setAttitude( rotation );
    }

    addChild( m_yTranslateAxis.get() );

    //Create translate z-axis dragger
    m_zTranslateAxis = new TranslateAxis();
    m_zTranslateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );
    m_zTranslateAxis->ComboForm();

    //Rotate z-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 0.0, 0.0, 1.0 ) );
        m_zTranslateAxis->setAttitude( rotation );
    }

    addChild( m_zTranslateAxis.get() );

    //Create translate pan dragger
    m_translatePan = new TranslatePan();
    m_translatePan->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 1.0, 1.0, 1.0 ), true );

    addChild( m_translatePan.get() );
}
////////////////////////////////////////////////////////////////////////////////
