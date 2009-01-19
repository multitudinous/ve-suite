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

// --- My Includes --- //
#include "DigitalGauge.h"
#include "Shaders.h"

// --- VE-Suite Includes --- //

// --- OSG Includes --- //
#include <osgText/Text>
#include <osgText/Font>

#include <osgDB/ReadFile>

// --- C/C++ Libraries --- //
#include <iostream>
#include <iomanip>
#include <sstream>

using namespace display;

////////////////////////////////////////////////////////////////////////////////
DigitalGauge::DigitalGauge()
:
osg::MatrixTransform(),
m_precision( 4 ),
m_name( "" )
{
    m_shader = new Shaders();

    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
DigitalGauge::DigitalGauge( std::string name )
:
osg::MatrixTransform(),
m_precision( 4 ),
m_name( name )
{
    m_shader = new Shaders();

    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
DigitalGauge::DigitalGauge( const DigitalGauge& digitalGauge, const osg::CopyOp& copyop )
:
osg::MatrixTransform( digitalGauge, copyop )
{
    *this = digitalGauge;
}
////////////////////////////////////////////////////////////////////////////////
DigitalGauge &DigitalGauge::operator=( const DigitalGauge &digitalGauge )
{ 
    if( this == &digitalGauge )
    {
        return *this;
    }

    m_precision = digitalGauge.m_precision;
    m_name = digitalGauge.m_name;

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
DigitalGauge::~DigitalGauge()
{
    delete m_shader;
}
////////////////////////////////////////////////////////////////////////////////
void DigitalGauge::Initialize()
{
    osg::ref_ptr< osg::Node > temp = osgDB::readNodeFile( "Models/digital_display.ive" );
    addChild( temp.get() );
    m_shader->Phong( temp.get() );

    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    m_nameText = new osgText::Text();
    m_digitalText = new osgText::Text();
    geode->addDrawable( m_nameText.get() );
    geode->addDrawable( m_digitalText.get() );
    addChild( geode.get() );

    std::string font( "fonts/arial.ttf" );

    m_nameText->setFont( font );
    m_nameText->setAxisAlignment( osgText::Text::SCREEN );
    m_nameText->setLayout( osgText::Text::LEFT_TO_RIGHT );
    m_nameText->setText( m_name );
    m_nameText->setPosition( osg::Vec3f( -0.55, 0.0, 0.2 ) );

    m_digitalText->setFont( font );
    m_digitalText->setAxisAlignment( osgText::Text::SCREEN );
    m_digitalText->setAlignment( osgText::Text::RIGHT_CENTER );
    m_digitalText->setLayout( osgText::Text::LEFT_TO_RIGHT );
    m_digitalText->setPosition( osg::Vec3f( 0.35, 0.0, 0.00 ) );
}
////////////////////////////////////////////////////////////////////////////////
void DigitalGauge::UpdateText( double value )
{
    std::stringstream ss;
    //ss << std::setw( 10 ) << std::setiosflags( std::ios_base::fixed );
    ss << std::setprecision( m_precision ) << std::setiosflags( std::ios_base::fixed ) << value;

    m_digitalText->setText( ss.str() );
}
////////////////////////////////////////////////////////////////////////////////
void DigitalGauge::SetPrecision( int precision )
{
    m_precision = precision;
}
////////////////////////////////////////////////////////////////////////////////
osgText::Text* DigitalGauge::GetNameText()
{
    return m_nameText.get();
}
////////////////////////////////////////////////////////////////////////////////
osgText::Text* DigitalGauge::GetDigitalText()
{
    return m_digitalText.get();
}
////////////////////////////////////////////////////////////////////////////////
