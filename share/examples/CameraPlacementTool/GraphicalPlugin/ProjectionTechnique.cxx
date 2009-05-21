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
#include "ProjectionTechnique.h"
#include "CameraEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
ProjectionTechnique::ProjectionTechnique()
    :
    ves::xplorer::scenegraph::technique::Technique(),
    mAlpha(
        new osg::Uniform( osg::Uniform::FLOAT, "alpha" ) ),
    mNearPlaneUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "nearPlane" ) ),
    mFarPlaneUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "farPlane" ) ),
    mFocalDistanceUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "focalDistance" ) ),
    mFocalRangeUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "focalRange" ) )
{
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
        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        stateset->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
        stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );
        stateset->setAttribute(
            ( ves::xplorer::scenegraph::ResourceManager::instance()->get
            < osg::Program, osg::ref_ptr >( "ProjectionProgram" ) ).get(),
            osg::StateAttribute::ON );

        stateset->addUniform( mAlpha.get() );
        stateset->addUniform( mNearPlaneUniform.get() );
        stateset->addUniform( mFarPlaneUniform.get() );
        stateset->addUniform( mFocalDistanceUniform.get() );
        stateset->addUniform( mFocalRangeUniform.get() );

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform* const ProjectionTechnique::GetAlpha() const
{
    return mAlpha.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform* const ProjectionTechnique::GetNearPlaneUniform() const
{
    return mNearPlaneUniform.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform* const ProjectionTechnique::GetFarPlaneUniform() const
{
    return mFarPlaneUniform.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform* const ProjectionTechnique::GetFocalDistanceUniform() const
{
    return mFocalDistanceUniform.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform* const ProjectionTechnique::GetFocalRangeUniform() const
{
    return mFocalRangeUniform.get();
}
////////////////////////////////////////////////////////////////////////////////
