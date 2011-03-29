/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>
#include <ves/xplorer/scenegraph/WCS.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

// --- BackdropFX Includes --- //
#include <backdropFX/Version.h>
#include <backdropFX/Manager.h>
#include <backdropFX/ShaderModule.h>
#include <backdropFX/ShaderModuleVisitor.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Depth>
#include <osg/Camera>

#include <osg/Light>
#include <osg/LightSource>
#include <osg/LightModel>

#include <osgText/Text>

#include <osgUtil/CullVisitor>

using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
HeadsUpDisplay::HeadsUpDisplay(
    std::pair< unsigned int, unsigned int > windowResolution )
    :
    mWindowResolution( windowResolution ),
    mCamera( 0 ),
    mFramerateTextGeode( 0 ),
    mGeometryWCS( 0 ),
    mFramerateText( 0 ),
    mWCSxText( 0 ),
    mWCSyText( 0 ),
    mWCSzText( 0 ),
    m_framerate( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
HeadsUpDisplay::~HeadsUpDisplay()
{
    if( mGeometryWCS )
    {
        delete mGeometryWCS;
    }
}
////////////////////////////////////////////////////////////////////////////////
void HeadsUpDisplay::Initialize()
{
    osg::ref_ptr< osg::Group > rootNode =
        ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();

    mCamera = new osg::Camera();
    mCamera->setName( "Heads Up Display Camera" );
    mCamera->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
    mCamera->setRenderOrder( osg::Camera::POST_RENDER, 1 );
    mCamera->setRenderTargetImplementation(
        osg::Camera::FRAME_BUFFER, osg::Camera::FRAME_BUFFER );
    //Need to do this for non RTT mode
    mCamera->setClearMask( GL_DEPTH_BUFFER_BIT );
    //mCamera->setClearMask( 0x00000000 );
    mCamera->setComputeNearFarMode(
        osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
    mCamera->setCullingActive( false );
    mCamera->setViewport(
        0, 0, mWindowResolution.first, mWindowResolution.second );
    mCamera->setViewMatrix( osg::Matrix::identity() );
    mCamera->setProjectionMatrix( osg::Matrix::ortho(
        0, mWindowResolution.first, 0, mWindowResolution.second, -50, 50 ) );
    rootNode->addChild( mCamera.get() );

    osg::ref_ptr< osg::Light > light = new osg::Light();
    light->setLightNum( 0 );
    light->setAmbient( osg::Vec4( 0.36862, 0.36842, 0.36842, 1.0 ) );
    light->setDiffuse( osg::Vec4( 0.88627, 0.88500, 0.88500, 1.0 ) );
    light->setSpecular( osg::Vec4( 0.49019, 0.48872, 0.48872, 1.0 ) );
    //We are in openGL space
    light->setPosition( osg::Vec4( 0.0, 10000.0, 10000.0, 0.0 ) );

    osg::ref_ptr< osg::LightSource > lightSource = new osg::LightSource();
    lightSource->setLight( light.get() );
    lightSource->setReferenceFrame( osg::LightSource::RELATIVE_RF );
    mCamera->addChild( lightSource.get() );

    osg::ref_ptr< osg::LightModel > lightModel = new osg::LightModel();
    lightModel->setAmbientIntensity( osg::Vec4( 0.1, 0.1, 0.1, 1.0 ) );
    //Get correct specular lighting across pipes
    //See http://www.ds.arch.tue.nl/General/Staff/Joran/osg/osg_specular_problem.htm
    lightModel->setLocalViewer( true );

    //Setup the light
    osg::ref_ptr< osg::StateSet > stateset = mCamera->getOrCreateStateSet();
    stateset->setAssociatedModes(
        light.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes(
        lightModel.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    std::string headsUpDisplayFont( "fonts/arial.ttf" );

    mFramerateTextGeode = new osg::Geode();
    mFramerateTextGeode->setNodeMask( false );
    mCamera->addChild( mFramerateTextGeode.get() );

    osg::ref_ptr< ves::xplorer::scenegraph::DCS > wcsDCS =
        new ves::xplorer::scenegraph::DCS();
    mGeometryWCS = new ves::xplorer::scenegraph::CADEntity(
        GetVESuite_WCS(), wcsDCS.get(), true, "Off" );
    mGeometryWCS->GetDCS()->setScale( osg::Vec3( 0.7, 0.7, 0.7 ) );
    mGeometryWCS->GetDCS()->setPosition( osg::Vec3(
        mWindowResolution.first - 50, mWindowResolution.second - 50, 0 ) );
    mGeometryWCS->GetDCS()->setNodeMask( false );
    mCamera->addChild( mGeometryWCS->GetDCS() );

    mFramerateText = new osgText::Text();
    mFramerateText->setFont( headsUpDisplayFont );
    mFramerateText->setCharacterSize( 20 );
    mFramerateText->setAxisAlignment( osgText::Text::SCREEN );
    mFramerateText->setAlignment( osgText::Text::RIGHT_BOTTOM );
    mFramerateText->setPosition(
        osg::Vec3( mWindowResolution.first - 10, 5, 0 ) );
    mFramerateTextGeode->addDrawable( mFramerateText.get() );

    mWCSxText = new osgText::Text();
    mWCSxText->setFont( headsUpDisplayFont );
    mWCSxText->setText( "x" );
    mWCSxText->setCharacterSize( 15 );
    mWCSxText->setAxisAlignment( osgText::Text::SCREEN );
    mWCSxText->setAlignment( osgText::Text::CENTER_CENTER );
    mWCSxText->setPosition( osg::Vec3( 50, 0, 0 ) );

    mWCSyText = new osgText::Text();
    mWCSyText->setFont( headsUpDisplayFont );
    mWCSyText->setText( "y" );
    mWCSyText->setCharacterSize( 15 );
    mWCSyText->setAxisAlignment( osgText::Text::SCREEN );
    mWCSyText->setAlignment( osgText::Text::CENTER_CENTER );
    mWCSyText->setPosition( osg::Vec3( 0, 0, -50 ) );

    mWCSzText = new osgText::Text();
    mWCSzText->setFont( headsUpDisplayFont );
    mWCSzText->setText( "z" );
    mWCSzText->setCharacterSize( 15 );
    mWCSzText->setAxisAlignment( osgText::Text::SCREEN );
    mWCSzText->setAlignment( osgText::Text::CENTER_CENTER );
    mWCSzText->setPosition( osg::Vec3( 0, 50, 0 ) );

    osg::ref_ptr< osg::Geode > wcsTextGeode = new osg::Geode();
    wcsTextGeode->addDrawable( mWCSxText.get() );
    wcsTextGeode->addDrawable( mWCSyText.get() );
    wcsTextGeode->addDrawable( mWCSzText.get() );
    mGeometryWCS->GetDCS()->addChild( wcsTextGeode.get() );

    {
        osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
        std::string fragmentSource =
        "uniform sampler2D baseMap; \n"

        "void main() \n"
        "{ \n"
            "vec4 texture = texture2D( baseMap, gl_TexCoord[ 0 ].xy ); \n"
            "gl_FragColor = mix( texture, gl_Color, texture.a ); \n"
        "} \n";

        fragmentShader->setType( osg::Shader::FRAGMENT );
        fragmentShader->setShaderSource( fragmentSource );
        fragmentShader->setName( "HUD Text Fragment Shader" );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragmentShader.get() );
        program->setName( "HUD Text Program" );

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        stateset->setAttributeAndModes(
            program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        //Here we attach the texture for the text
        stateset->addUniform( new osg::Uniform( "baseMap", 0 ) );

        mFramerateTextGeode->setStateSet( stateset.get() );
        wcsTextGeode->setStateSet( stateset.get() );
    }
    
    //Create shader modules emulating ffp
    if( !scenegraph::SceneManager::instance()->IsRTTOn() )
    {
        backdropFX::ShaderModuleVisitor smv;
        smv.setAddDefaults( false );
        
        SetFrameRateFlag( true );
        SetCoordSysFlag( true );
        
        mCamera->accept( smv );
        
        SetFrameRateFlag( false );
        SetCoordSysFlag( false );
    }    
}
////////////////////////////////////////////////////////////////////////////////
void HeadsUpDisplay::LatePreFrame()
{
    if( (mFramerateTextGeode->getNodeMask() == 0) && 
        (mGeometryWCS->GetDCS()->getNodeMask() == 0) )
    {
        return;
    }

    osg::ref_ptr< osg::Group > activeDCS =
        ves::xplorer::scenegraph::SceneManager::instance()->GetActiveSwitchNode();
    osg::ref_ptr< osg::Group > worldDCS =
        ves::xplorer::scenegraph::SceneManager::instance()->GetModelRoot();

    if( activeDCS != worldDCS )
    {
        mCamera->setNodeMask( 0 );
    }
    else
    {
        mCamera->setNodeMask( 1 );
    }

    if( mFramerateTextGeode->getNodeMask() != 0 )
    {
        mFps.str( "" );
        mFps << m_framerate;
        mFps << " fps";

        mFramerateText->setText( mFps.str() );
    }

    if( mGeometryWCS->GetDCS()->getNodeMask() != 0 )
    {
        osg::Quat temp = ves::xplorer::scenegraph::SceneManager::instance()->
            GetNavDCS()->getAttitude();
        osg::Quat quat( temp.x(), temp.z(), -temp.y(), temp.w() );

        mGeometryWCS->GetDCS()->setAttitude( quat );
    }
}
////////////////////////////////////////////////////////////////////////////////
std::pair< unsigned int, unsigned int > HeadsUpDisplay::GetWindowResolution()
{
    return mWindowResolution;
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* HeadsUpDisplay::GetCamera()
{
    return mCamera.get();
}
////////////////////////////////////////////////////////////////////////////////
void HeadsUpDisplay::SetFrameRateFlag( bool val )
{
    mFramerateTextGeode->setNodeMask( val );
}
////////////////////////////////////////////////////////////////////////////////
void HeadsUpDisplay::SetCoordSysFlag( bool val )
{
    mGeometryWCS->GetDCS()->setNodeMask( val );
}
////////////////////////////////////////////////////////////////////////////////
void HeadsUpDisplay::SetTextColor( std::vector< double > color )
{
    if( ( color[ 0 ] + color[ 1 ] + color[ 2 ] ) > 1.1 &&
        ( color[ 0 ] + color[ 1 ] + color[ 2 ] ) < 2.0 )
    {
        mFramerateText->setColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
        mWCSxText->setColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
        mWCSyText->setColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
        mWCSzText->setColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    }
    else
    {
        mFramerateText->setColor( osg::Vec4( ( 1.0 - color[ 0 ] ),
                                             ( 1.0 - color[ 1 ] ),
                                             ( 1.0 - color[2] ), 1.0 ) );
        mWCSxText->setColor( osg::Vec4( ( 1.0 - color[ 0 ] ),
                                        ( 1.0 - color[ 1 ] ),
                                        ( 1.0 - color[ 2 ] ), 1.0 ) );
        mWCSyText->setColor( osg::Vec4( ( 1.0 - color[ 0 ] ),
                                        ( 1.0 - color[ 1 ] ),
                                        ( 1.0 - color[ 2 ] ), 1.0 ) );
        mWCSzText->setColor( osg::Vec4( ( 1.0 - color[ 0 ] ),
                                        ( 1.0 - color[ 1 ] ),
                                        ( 1.0 - color[ 2 ] ), 1.0 ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void HeadsUpDisplay::SetFrameRate( const float frameRate )
{
    m_framerate = frameRate;
}