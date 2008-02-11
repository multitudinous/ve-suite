/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
#ifdef _OSG
#include <osg/StateSet>
#include <osg/Material>
#include <osg/BlendFunc>
#include <osg/ShadeModel>
#elif _PERFORMER
#endif

#include <ves/xplorer/scenegraph/util/MaterialHelper.h>

#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/FloatArrayPtr.h>
#include <string>
using namespace ves::open::xml::cad;
using namespace ves::xplorer::scenegraph::util;
////////////////////////////////
//Construtor                  //
////////////////////////////////
MaterialHelper::MaterialHelper()
{}
/////////////////////////////////
//Destructor                   //
/////////////////////////////////
MaterialHelper::~MaterialHelper()
{}
////////////////////////////////////////////////////////////////
void MaterialHelper::LoadMaterial( CADMaterialPtr material )
{
#ifdef _OSG
    if( !m_material.valid() )
    {
        m_material = new osg::Material();
    }
    if( !m_ss.valid() )
    {
        m_ss = new osg::StateSet();
    }
    else
    {
        m_ss->clear();
    }

    std::string materialName = material->GetMaterialName();
    //std::cout<<"Loading material: "<<materialName<<std::endl;

    std::vector<double> diffuse = material->GetDiffuse()->GetArray();
    std::vector<double> ambient = material->GetAmbient()->GetArray();
    std::vector<double> specular = material->GetSpecular()->GetArray();
    std::vector<double> emmissive = material->GetEmissive()->GetArray();
    double opacity = material->GetOpacity();
    std::string face = material->GetFace();
    std::string colorMode = material->GetColorMode();
    float shininess = material->GetShininess();

    osg::Material::ColorMode cMode =
        osg::Material::AMBIENT_AND_DIFFUSE;

    if( colorMode == std::string( "Diffuse" ) )
    {
        cMode = osg::Material::DIFFUSE;
    }
    else if( colorMode == std::string( "Ambient" ) )
    {
        osg::Material::AMBIENT;
    }
    else if( colorMode == std::string( "Emissive" ) )
    {
        osg::Material::EMISSION;
    }
    else if( colorMode == std::string( "Specular" ) )
    {
        osg::Material::SPECULAR;
    }
    else if( colorMode == std::string( "Ambient_and_Diffuse" ) )
    {
        osg::Material::AMBIENT_AND_DIFFUSE;
    }
    else if( colorMode == std::string( "Off" ) )
    {
        osg::Material::OFF;
    }
    else
    {
        std::cout << "Unrecognized color mode: " << colorMode << std::endl;
        std::cout << "SceneGraphBuilderSceneGraphCallback::Apply()" << std::endl;
    }
    m_material->setColorMode( cMode );

    osg::Material::Face faceToApply = osg::Material::FRONT;
    if( face == std::string( "Front_and_Back" ) )
    {
        faceToApply = osg::Material::FRONT_AND_BACK;
    }
    else if( face == std::string( "Back" ) )
    {
        faceToApply = osg::Material::BACK;
    }
    else if( face == std::string( "Front" ) )
    {
        faceToApply = osg::Material::FRONT;
    }
    else
    {
        std::cout << "Unrecognized face: " << face << std::endl;
        std::cout << "SceneGraphBuilderSceneGraphCallback::Apply()" << std::endl;
    }
    m_material->setDiffuse( faceToApply,
                           osg::Vec4( diffuse.at( 0 ), diffuse.at( 1 ), diffuse.at( 2 ), opacity ) );

    m_material->setAmbient( faceToApply,
                           osg::Vec4( ambient.at( 0 ), ambient.at( 1 ), ambient.at( 2 ), opacity ) );

    m_material->setEmission( faceToApply,
                            osg::Vec4( emmissive.at( 0 ), emmissive.at( 1 ), emmissive.at( 2 ), opacity ) );

    m_material->setSpecular( faceToApply,
                            osg::Vec4( specular.at( 0 ), specular.at( 1 ), specular.at( 2 ), opacity ) );

    //_material->setName(materialName);
    m_material->setShininess( faceToApply, shininess );
    m_ss->setAttributeAndModes( m_material.get(), osg::StateAttribute::ON );

    osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;

    bf->setFunction( osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA );

    if( opacity == 1.0 )
    {
        m_ss->setRenderingHint( osg::StateSet::OPAQUE_BIN );
        m_ss->setMode( GL_BLEND, osg::StateAttribute::ON );
    }
    else
    {
        m_ss->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );
        m_ss->setRenderBinDetails( 99, std::string( "DepthSortedBin" ) );
        m_ss->setMode( GL_BLEND, osg::StateAttribute::ON );
    }
    m_ss->setAttributeAndModes( bf.get(), osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    m_ss->setAttributeAndModes( m_material.get(), osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
#elif _PERFORMER
    std::cout << "Material Loader Not implemented for Performer version yet!!!!!!" << std::endl;
#endif
}
#ifdef _OSG
/////////////////////////////////////////////////////////////////
osg::ref_ptr<osg::StateSet> MaterialHelper::GetMaterialStateSet()
{
    if( m_ss.valid() )
    {
        return m_ss;
    }
    return 0;
}
/////////////////////////////////////////////////////////
void MaterialHelper::SetStateSet( osg::StateSet* material )
{
    m_ss = material;
}
#elif _PERFORMER
#endif



