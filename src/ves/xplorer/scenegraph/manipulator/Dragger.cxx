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
 * Date modified: $Date: 2009-05-06 14:32:42 -0600 (Wed, 06 May 2009) $
 * Version:       $Rev: 12657 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: Dragger.cxx 12657 2009-05-06 20:32:42Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //


using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger()
    :
    osg::MatrixTransform()
{
    m_colorMap[ ColorTag::DEFAULT ] = osg::Vec4f( 0.0, 0.0, 0.0, 1.0 );
    m_colorMap[ ColorTag::FOCUS ] = osg::Vec4f( 1.0, 1.0, 0.0, 1.0 );
    m_colorMap[ ColorTag::ACTIVE ] = osg::Vec4f( 1.0, 1.0, 1.0, 1.0 );
    m_colorMap[ ColorTag::OTHER ] = osg::Vec4f( 0.0, 0.0, 0.0, 1.0 );

    m_color = new osg::Uniform( "color", GetColor( ColorTag::DEFAULT ) );

    CreateDefaultShader();
}
////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger(
    const Dragger& dragger, const osg::CopyOp& copyop )
    :
    osg::MatrixTransform( dragger, copyop ),
    m_colorMap( dragger.m_colorMap ),
    m_color( dragger.m_color )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Dragger::~Dragger()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Dragger::Handle( Event::Enum event )
{
    //std::find( 
    //if( ( this ) )
    {
        UseColor( ColorTag::DEFAULT );

        return false;
    }

    switch( event )
    {
        case Event::FOCUS:
        {
            UseColor( ColorTag::FOCUS );

            return true;
        }
        case Event::PUSH:
        {
            UseColor( ColorTag::ACTIVE );

            return true;
        }
        case Event::DRAG:
        {
            return true;
        }
        case Event::RELEASE:
        {
            UseColor( ColorTag::DEFAULT );

            return true;
        }
        default:
        {
            return false;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetupDefaultGeometry()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetDrawableToAlwaysCull( osg::Drawable& drawable )
{
    osg::ref_ptr< Dragger::ForceCullCallback > forceCullCallback =
        new Dragger::ForceCullCallback();
    drawable.setCullCallback( forceCullCallback.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::CreateDefaultShader()
{
    //Create the shader used to render the dragger
    std::string fragmentSource =
    "uniform vec4 color; \n"

    "void main() \n"
    "{ \n"
        "gl_FragColor = color; \n"
    "} \n";

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragmentShader.get() );

    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();
    stateSet->setAttributeAndModes( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    stateSet->addUniform( m_color.get() );
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec4& Dragger::GetColor( ColorTag::Enum colorTag )
{
    std::map< ColorTag::Enum, osg::Vec4 >::iterator itr =
        m_colorMap.find( colorTag );
    /*
    if( itr == m_colorMap.end() )
    {
        //error handling, but shouldn't need it since we know colors
    }
    */

    return itr->second;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetColor( ColorTag::Enum colorTag, osg::Vec4& newColor, bool use )
{
    osg::Vec4& color = GetColor( colorTag );
    if( color == newColor )
    {
        return;
    }

    color = newColor;

    if( use )
    {
        m_color->set( color );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::TurnOn()
{
    setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::TurnOff()
{
    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::UseColor( ColorTag::Enum colorTag )
{
    m_color->set( GetColor( colorTag ) );
}
////////////////////////////////////////////////////////////////////////////////
Dragger::ForceCullCallback::ForceCullCallback()
    :
    osg::Drawable::CullCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Dragger::ForceCullCallback::ForceCullCallback(
    const ForceCullCallback& forceCullCallback,
    const osg::CopyOp& copyop )
    :
    osg::Drawable::CullCallback( forceCullCallback, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Dragger::ForceCullCallback::cull(
    osg::NodeVisitor* nv,
    osg::Drawable* drawable,
    osg::RenderInfo* renderInfo ) const
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
