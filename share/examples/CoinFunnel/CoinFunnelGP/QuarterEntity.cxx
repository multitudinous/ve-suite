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
#include "QuarterEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/Texture2D>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
QuarterEntity::QuarterEntity( std::string geomFile,
                              ves::xplorer::scenegraph::DCS* pluginDCS,
                              ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
CADEntity( geomFile, pluginDCS, false, false, physicsSimulator )
{
    mNonPhysicsGeometry = osgDB::readNodeFile( "Models/IVEs/quarter_front.ive" );
    mNonPhysicsGeometryII = osgDB::readNodeFile( "Models/IVEs/quarter_back.ive" );

    GetDCS()->addChild( mNonPhysicsGeometry.get() );
    GetDCS()->addChild( mNonPhysicsGeometryII.get() );
}
////////////////////////////////////////////////////////////////////////////////
QuarterEntity::~QuarterEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void QuarterEntity::SetNameAndDescriptions( const std::string& geomFile )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( geomFile );
}
////////////////////////////////////////////////////////////////////////////////
void QuarterEntity::SetShaders()
{
    osg::ref_ptr< osg::Texture2D > quarterEdge =
        new osg::Texture2D( osgDB::readImageFile( "Textures/QuarterEdge.jpg" ) );
    quarterEdge->setWrap( osg::Texture::WRAP_S, osg::Texture::REPEAT );
    quarterEdge->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_BORDER );

    osg::ref_ptr< osg::Texture2D > quarterFront =
        new osg::Texture2D( osgDB::readImageFile( "Textures/QuarterFront.jpg" ) );
    osg::ref_ptr< osg::Texture2D > quarterBack =
        new osg::Texture2D( osgDB::readImageFile( "Textures/QuarterBack.jpg" ) );

    SetShaderOne( GetNode()->GetNode(), quarterEdge.get() );
    SetShaderOne( mNonPhysicsGeometry.get(), quarterFront.get() );
    SetShaderOne( mNonPhysicsGeometryII.get(), quarterBack.get() );
}
////////////////////////////////////////////////////////////////////////////////
void QuarterEntity::SetShaderOne( osg::Node* node, osg::Texture2D* texture )
{
    char vertexPass[]=
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "gl_TexCoord[ 0 ].xy = gl_MultiTexCoord0.xy; \n"
            "eyePos = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
        "} \n";

    char fragmentPass[]=
        "uniform sampler2D baseColor; \n"

        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "vec3 N = normalize( normal ); \n"
            "vec3 L = normalize( lightPos ); \n"
            "float NDotL = max( dot( N, L ), 0.0 ); \n"

            "vec3 V = normalize( eyePos ); \n"
            "vec3 R = reflect( V, N ); \n"
            "float RDotL = max( dot( R, L ), 0.0 ); \n"

            "vec3 color = vec3( texture2D( baseColor, gl_TexCoord[ 0 ].xy ) ); \n"

            "vec3 TotalAmbient  = gl_LightSource[ 0 ].ambient.rgb  * color; \n"
            "vec3 TotalDiffuse  = gl_LightSource[ 0 ].diffuse.rgb  * color * NDotL; \n"
            "vec3 TotalSpecular = gl_LightSource[ 0 ].specular.rgb * color * pow( RDotL, 15.0 ); \n"

            "gl_FragColor = vec4( TotalAmbient + TotalDiffuse + TotalSpecular, 1.0 ); \n"
        "} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    stateset->setAttribute( program.get(), osg::StateAttribute::ON );

    stateset->setTextureAttributeAndModes( 0, texture, osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > baseColor = new osg::Uniform( "baseColor", 0 );
    stateset->addUniform( baseColor.get() );
        
    node->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo