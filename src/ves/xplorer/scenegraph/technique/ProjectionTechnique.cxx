/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/scenegraph/technique/ProjectionTechnique.h>

// --- OSG Includes --- //
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace technique
{

////////////////////////////////////////////////////////////////////////////////
ProjectionTechnique::ProjectionTechnique()
    :
    ves::xplorer::scenegraph::technique::Technique(),
    m_alpha(
        new osg::Uniform( osg::Uniform::FLOAT, "alpha" ) ),
    m_nearPlaneUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "nearPlane" ) ),
    m_farPlaneUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "farPlane" ) ),
    m_focalDistanceUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "focalDistance" ) ),
    m_focalRangeUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "focalRange" ) ),
    m_pictureFrameUniform(
        new osg::Uniform( osg::Uniform::BOOL, "cameraPictureFrame" ) )
{
    m_pictureFrameUniform->set( false );

    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
ProjectionTechnique::~ProjectionTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ProjectionTechnique::DefinePasses()
{
    //Implement pass #1
    {
        std::string vsName = osgDB::findDataFile( "projection.vs" );
        std::string fsName = osgDB::findDataFile( "projection.fs" );

        osg::ref_ptr< osg::Shader > vs =
            osg::Shader::readShaderFile( osg::Shader::VERTEX, vsName );

        osg::ref_ptr< osg::Shader > fs =
            osg::Shader::readShaderFile( osg::Shader::FRAGMENT, fsName );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        //program->addShader( vs.get() );
        program->addShader( fs.get() );

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        stateset->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
        stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
        stateset->setTextureMode( 6, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
        stateset->setTextureMode( 6, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
        stateset->setTextureMode( 6, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );
        stateset->setAttribute(
            program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );

        stateset->addUniform( m_alpha.get() );
        stateset->addUniform( m_nearPlaneUniform.get() );
        stateset->addUniform( m_farPlaneUniform.get() );
        stateset->addUniform( m_focalDistanceUniform.get() );
        stateset->addUniform( m_focalRangeUniform.get() );
        stateset->addUniform( m_pictureFrameUniform.get() );

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ProjectionTechnique::SetAlpha( float const& alpha )
{
    m_alpha->set( alpha );
}
////////////////////////////////////////////////////////////////////////////////
void ProjectionTechnique::SetNearPlane( float const& nearPlane )
{
    m_nearPlaneUniform->set( nearPlane );
}
////////////////////////////////////////////////////////////////////////////////
void ProjectionTechnique::SetFarPlane( float const& farPlane )
{
    m_farPlaneUniform->set( farPlane );
}
////////////////////////////////////////////////////////////////////////////////
void ProjectionTechnique::SetFocalDistance( float const& focalDistance )
{
    m_focalDistanceUniform->set( focalDistance );
}
////////////////////////////////////////////////////////////////////////////////
void ProjectionTechnique::SetFocalRange( float const& focalRange )
{
    m_focalRangeUniform->set( focalRange );
}
////////////////////////////////////////////////////////////////////////////////
void ProjectionTechnique::SetPictureFrame( bool pictureFrame )
{
    m_pictureFrameUniform->set( pictureFrame );
}
////////////////////////////////////////////////////////////////////////////////
} //end technique
} //end scenegraph
} //end xplorer
} //end ves
