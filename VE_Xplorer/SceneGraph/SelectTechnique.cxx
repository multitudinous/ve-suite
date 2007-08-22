/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/SelectTechnique.h"
#include "VE_Xplorer/SceneGraph/DCS.h"

// --- OSG Includes --- //
#include <osg/BlendFunc>
#include <osg/CameraNode>
#include <osg/Texture2D>
#include <osg/LineWidth>
#include <osg/Material>
#include <osg/PolygonMode>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
SelectTechnique::SelectTechnique( VE_SceneGraph::DCS* dcs )
:
m_dcs( dcs )
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
SelectTechnique::~SelectTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SelectTechnique::DefinePasses()
{
    //Implement pass #1
    {
        osg::ref_ptr< osg::StateSet > stateset = m_dcs->getOrCreateStateSet();

        AddPass( stateset.get() );
    }

    // implement pass #2
    {
        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        osg::ref_ptr< osg::LineWidth > linewidth = new osg::LineWidth();
        osg::ref_ptr< osg::Material > material = new osg::Material();
        osg::ref_ptr< osg::PolygonMode > polymode = new osg::PolygonMode();

        linewidth->setWidth( 2.0f );
        material->setColorMode( osg::Material::OFF );
        material->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        material->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        material->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        material->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 1.0f, 0.0f, 1.0f ) );
        polymode->setMode( osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );
        
        stateset->setAttributeAndModes( linewidth.get(), osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON );
        stateset->setAttributeAndModes( material.get(), osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON );
        stateset->setAttributeAndModes( polymode.get(), osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON );

        stateset->setMode( GL_LIGHTING, osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_1D, osg::StateAttribute::OVERRIDE | osg::StateAttribute::OFF );
        stateset->setTextureMode( 0, GL_TEXTURE_2D, osg::StateAttribute::OVERRIDE | osg::StateAttribute::OFF );

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
