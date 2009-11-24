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
#include <ves/xplorer/scenegraph/technique/ParallaxMapping.h>

#include <ves/xplorer/scenegraph/util/TSGVisitor.h>

// --- OSG Includes --- //
#include <osg/Texture2D>

#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

using namespace ves::xplorer::scenegraph::technique;

////////////////////////////////////////////////////////////////////////////////
ParallaxMapping::ParallaxMapping( osg::Node* node )
    :
    Technique(),
    m_node( node ),
    m_stateSet( NULL ),
    m_baseMap( NULL ),
    m_normalMap( NULL ),
    m_heightMap( NULL )
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
ParallaxMapping::~ParallaxMapping()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMapping::DefinePasses()
{
    //Implement pass #1
    //Render the geometry with current StateSet
    {
        m_stateSet = new osg::StateSet();

        m_baseMap = new osg::Texture2D();
        m_baseMap->setFilter(
            osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
        m_baseMap->setFilter(
            osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
        m_baseMap->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::REPEAT );
        m_baseMap->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::REPEAT );
        m_stateSet->setTextureAttributeAndModes(
            0, m_baseMap.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Uniform > baseMapUniform =
            new osg::Uniform( "baseMap", 0 );
        m_stateSet->addUniform( baseMapUniform.get() );

        m_normalMap = new osg::Texture2D();
        m_normalMap->setFilter(
            osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
        m_normalMap->setFilter(
            osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
        m_normalMap->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::REPEAT );
        m_normalMap->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::REPEAT );
        m_stateSet->setTextureAttributeAndModes(
            1, m_normalMap.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Uniform > normalMapUniform =
            new osg::Uniform( "normalMap", 1 );
        m_stateSet->addUniform( normalMapUniform.get() );

        m_heightMap = new osg::Texture2D();
        m_heightMap->setFilter(
            osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
        m_heightMap->setFilter(
            osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
        m_heightMap->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::REPEAT );
        m_heightMap->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::REPEAT );
        m_stateSet->setTextureAttributeAndModes(
            2, m_heightMap.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Uniform > heightMapUniform =
            new osg::Uniform( "heightMap", 2 );
        m_stateSet->addUniform( heightMapUniform.get() );

        //osg::ref_ptr< osgDB::ReaderWriter::Options > vertexOptions =
        //    new osgDB::ReaderWriter::Options( "vertex" );
        //osg::ref_ptr< osgDB::ReaderWriter::Options > fragmentOptions =
        //    new osgDB::ReaderWriter::Options( "fragment" );

        //osg::ref_ptr< osg::Shader > vertexShader = osgDB::readShaderFile(
        //    "glsl/parallax_mapping.vs", vertexOptions.get() );
        //osg::ref_ptr< osg::Shader > fragmentShader = osgDB::readShaderFile(
        //    "glsl/parallax_mapping.fs", fragmentOptions.get() );

        std::string shaderName = osgDB::findDataFile( "parallax_mapping.fs" );
        osg::ref_ptr< osg::Shader > fragmentShader = 
            osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );
        shaderName = osgDB::findDataFile( "parallax_mapping.vs" );
        osg::ref_ptr< osg::Shader > vertexShader = 
            osg::Shader::readShaderFile( osg::Shader::VERTEX, shaderName );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( vertexShader.get() );
        program->addShader( fragmentShader.get() );
        program->addBindAttribLocation( "a_tangent", 6 );
        program->addBindAttribLocation( "a_binormal", 7 );
        program->addBindAttribLocation( "a_normal", 15 );
        m_stateSet->setAttribute( program.get(), osg::StateAttribute::ON );

        AddPass( m_stateSet.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMapping::SetBaseMap( osg::Image* const image )
{
    m_baseMap->setImage( image );
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMapping::SetNormalMap( osg::Image* const image )
{
    m_normalMap->setImage( image );

    ves::xplorer::scenegraph::util::TSGVisitor tsgVisitor( m_node, 0 );
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMapping::SetHeightMap( osg::Image* const image )
{
    m_heightMap->setImage( image );
}
////////////////////////////////////////////////////////////////////////////////