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
 * Date modified: $Date: 2009-05-14 10:29:09 -0600 (Thu, 14 May 2009) $
 * Version:       $Rev: 12686 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: TranslateCompound.cxx 12686 2009-05-14 16:29:09Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/TranslateCompound.h>
#include <ves/xplorer/scenegraph/manipulator/TranslateAxis.h>
#include <ves/xplorer/scenegraph/manipulator/TranslatePan.h>

// --- OSG Includes --- //

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TranslateCompound::TranslateCompound()
    :
    CompoundDragger(),
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
void TranslateCompound::SetupDefaultGeometry()
{
    //Create translate x-axis dragger
    m_xTranslateAxis = new TranslateAxis();
    m_xTranslateAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );

    addChild( m_xTranslateAxis.get() );

    //Create translate y-axis dragger
    m_yTranslateAxis = new TranslateAxis();
    m_yTranslateAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );

    //Rotate y-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate(
            osg::Vec3d( 1.0, 0.0, 0.0 ), osg::Vec3d( 0.0, 1.0, 0.0 ) );
        m_yTranslateAxis->setMatrix( osg::Matrix( rotation ) );
    }

    addChild( m_yTranslateAxis.get() );

    //Create translate z-axis dragger
    m_zTranslateAxis = new TranslateAxis();
    m_zTranslateAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );

    //Rotate z-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate(
            osg::Vec3d( 1.0, 0.0, 0.0 ), osg::Vec3d( 0.0, 0.0, 1.0 ) );
        m_zTranslateAxis->setMatrix( osg::Matrix( rotation ) );
    }

    addChild( m_zTranslateAxis.get() );

    //Create translate pan dragger
    m_translatePan = new TranslatePan();
    m_translatePan->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 1.0, 1.0, 1.0, 1.0 ), true );

    addChild( m_translatePan.get() );
}
////////////////////////////////////////////////////////////////////////////////
