// --- My Includes --- //
#include "DigitalGauge.h"
#include "Shaders.h"

// --- VE-Suite Includes --- //

// --- OSG Includes --- //
#include <osg/CameraNode>

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
    temp->setStateSet( m_shader->Phong().get() );

    osg::ref_ptr< osg::CameraNode > cameraNode = new osg::CameraNode();
    cameraNode->setReferenceFrame( osg::Transform::RELATIVE_RF );
    cameraNode->setViewMatrix( osg::Matrix::identity() );
    cameraNode->setClearMask( GL_DEPTH_BUFFER_BIT );
    cameraNode->setRenderOrder( osg::CameraNode::POST_RENDER );

    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    cameraNode->addChild( geode.get() );

    m_nameText = new osgText::Text();
    m_digitalText = new osgText::Text();
    geode->addDrawable( m_nameText.get() );
    geode->addDrawable( m_digitalText.get() );
    addChild( cameraNode.get() );

    std::string font( "fonts/arial.ttf" );

    m_nameText->setFont( font );
    m_nameText->setAxisAlignment( osgText::Text::SCREEN );
    m_nameText->setLayout( osgText::Text::LEFT_TO_RIGHT );
    m_nameText->setText( m_name );

    m_digitalText->setFont( font );
    m_digitalText->setAxisAlignment( osgText::Text::SCREEN );
    m_digitalText->setLayout( osgText::Text::LEFT_TO_RIGHT );
}
////////////////////////////////////////////////////////////////////////////////
void DigitalGauge::UpdateText( double value )
{
    std::stringstream ss;
    ss << std::setw( 10 ) << std::setiosflags( std::ios_base::fixed );
    ss << std::setprecision( m_precision ) << value;

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