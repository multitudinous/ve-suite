/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/open/xml/cad/CADAttribute.h>
#include <ves/open/xml/shader/Uniform.h>
#include <ves/xplorer/scenegraph/util/MaterialHelper.h>
#include <ves/xplorer/scenegraph/util/ShaderHelper.h>
#include <ves/xplorer/scenegraph/util/Attribute.h>

//why is this needed
#include <osg/Material>
#include <osg/BlendFunc>

//#include <osg/Texture2D>
#include <osg/TextureCubeMap>
#include <osg/TextureRectangle>

#include <osgDB/Registry>
#include <osgDB/Input>
#include <osgDB/Output>

#include <set>

using namespace osg;
using namespace osgDB;
using namespace std;

using namespace ves::xplorer::scenegraph::util;
using namespace ves::open::xml::cad;
using namespace ves::open::xml::shader;

bool VEAttribute_readLocalData( Object& obj, Input& fr );
bool VEAttribute_writeLocalData( const Object& obj, Output& fw );

bool VEAttribute_matchModeStr( const char* str, StateAttribute::GLModeValue& mode );
const char* VEAttribute_getModeStr( StateAttribute::GLModeValue mode );

bool VEAttribute_matchRenderBinModeStr( const char* str, StateSet::RenderBinMode& mode );
const char* VEAttribute_getRenderBinModeStr( StateSet::RenderBinMode mode );

// register the read and write functions with the osgDB::Registry.
RegisterDotOsgWrapperProxy ve_AttributeFuncProxy
(
    new ves::xplorer::scenegraph::util::Attribute(),
    "Attribute",
    "Object ves::xplorer::scenegraph::util::Attribute",
    &VEAttribute_readLocalData,
    &VEAttribute_writeLocalData,
    DotOsgWrapper::READ_AND_WRITE
);
//
// Set up the maps from name to GLMode and visa versa.
//
typedef std::map<std::string, StateAttribute::GLMode>    VEGLNameToGLModeMap;
typedef std::map<StateAttribute::GLMode, std::string>    VEGLModeToGLNameMap;
typedef std::set<StateAttribute::GLMode>                VETextureGLModeSet;

VEGLNameToGLModeMap ve_GLNameToGLModeMap;
VEGLModeToGLNameMap ve_GLModeToGLNameMap;
VETextureGLModeSet ve_TextureGLModeSet;

#define VEADD_NAME(name,mode) ve_GLNameToGLModeMap[name]=mode; ve_GLModeToGLNameMap[mode]=name;

void ve_initGLNames()
{
    static bool first_time = true;
    if( !first_time ) return;

    VEADD_NAME( "GL_ALPHA_TEST", GL_ALPHA_TEST )
    VEADD_NAME( "GL_BLEND", GL_BLEND )
    VEADD_NAME( "GL_COLOR_MATERIAL", GL_COLOR_MATERIAL )
    VEADD_NAME( "GL_CULL_FACE", GL_CULL_FACE )
    VEADD_NAME( "GL_DEPTH_TEST", GL_DEPTH_TEST )
    VEADD_NAME( "GL_FOG", GL_FOG )
    VEADD_NAME( "GL_LIGHTING", GL_LIGHTING )
    VEADD_NAME( "GL_POINT_SMOOTH", GL_POINT_SMOOTH )
    VEADD_NAME( "GL_LINE_STIPPLE", GL_LINE_STIPPLE )
    VEADD_NAME( "GL_POLYGON_OFFSET_FILL", GL_POLYGON_OFFSET_FILL )
    VEADD_NAME( "GL_POLYGON_OFFSET_LINE", GL_POLYGON_OFFSET_LINE )
    VEADD_NAME( "GL_POLYGON_OFFSET_POINT", GL_POLYGON_OFFSET_POINT )
    VEADD_NAME( "GL_COLOR_SUM", GL_COLOR_SUM );

    VEADD_NAME( "GL_TEXTURE_1D", GL_TEXTURE_1D )
    VEADD_NAME( "GL_TEXTURE_2D", GL_TEXTURE_2D )
    VEADD_NAME( "GL_TEXTURE_3D", GL_TEXTURE_3D )

    VEADD_NAME( "GL_TEXTURE_CUBE_MAP", GL_TEXTURE_CUBE_MAP );
    VEADD_NAME( "GL_TEXTURE_RECTANGLE", GL_TEXTURE_RECTANGLE );

    VEADD_NAME( "GL_TEXTURE_GEN_Q", GL_TEXTURE_GEN_Q )
    VEADD_NAME( "GL_TEXTURE_GEN_R", GL_TEXTURE_GEN_R )
    VEADD_NAME( "GL_TEXTURE_GEN_S", GL_TEXTURE_GEN_S )
    VEADD_NAME( "GL_TEXTURE_GEN_T", GL_TEXTURE_GEN_T )

    VEADD_NAME( "GL_STENCIL_TEST", GL_STENCIL_TEST )

    VEADD_NAME( "GL_CLIP_PLANE0", GL_CLIP_PLANE0 );
    VEADD_NAME( "GL_CLIP_PLANE1", GL_CLIP_PLANE1 );
    VEADD_NAME( "GL_CLIP_PLANE2", GL_CLIP_PLANE2 );
    VEADD_NAME( "GL_CLIP_PLANE3", GL_CLIP_PLANE3 );
    VEADD_NAME( "GL_CLIP_PLANE4", GL_CLIP_PLANE4 );
    VEADD_NAME( "GL_CLIP_PLANE5", GL_CLIP_PLANE5 );

    VEADD_NAME( "GL_LIGHT0", GL_LIGHT0 );
    VEADD_NAME( "GL_LIGHT1", GL_LIGHT1 );
    VEADD_NAME( "GL_LIGHT2", GL_LIGHT2 );
    VEADD_NAME( "GL_LIGHT3", GL_LIGHT3 );
    VEADD_NAME( "GL_LIGHT4", GL_LIGHT4 );
    VEADD_NAME( "GL_LIGHT5", GL_LIGHT5 );
    VEADD_NAME( "GL_LIGHT6", GL_LIGHT6 );
    VEADD_NAME( "GL_LIGHT7", GL_LIGHT7 );

    ve_TextureGLModeSet.insert( GL_TEXTURE_1D );
    ve_TextureGLModeSet.insert( GL_TEXTURE_2D );
    ve_TextureGLModeSet.insert( GL_TEXTURE_3D );

    ve_TextureGLModeSet.insert( GL_TEXTURE_CUBE_MAP );
    ve_TextureGLModeSet.insert( GL_TEXTURE_RECTANGLE );

    ve_TextureGLModeSet.insert( GL_TEXTURE_GEN_Q );
    ve_TextureGLModeSet.insert( GL_TEXTURE_GEN_R );
    ve_TextureGLModeSet.insert( GL_TEXTURE_GEN_S );
    ve_TextureGLModeSet.insert( GL_TEXTURE_GEN_T );

    first_time = false;
}

bool VEAttribute_readLocalData( Object& obj, Input& fr )
{
    bool iteratorAdvanced = false;

    // note, StateSet replaced GeoState April 2001.
    Attribute& stateset = static_cast<Attribute&>( obj );

    ve_initGLNames();

    // read the rendering hint value.
    if( fr[0].matchWord( "rendering_hint" ) )
    {
        if( fr[1].matchWord( "DEFAULT_BIN" ) )
        {
            stateset.setRenderingHint( StateSet::DEFAULT_BIN );
            fr += 2;
            iteratorAdvanced = true;
        }
        else if( fr[1].matchWord( "OPAQUE_BIN" ) )
        {
            stateset.setRenderingHint( StateSet::OPAQUE_BIN );
            fr += 2;
            iteratorAdvanced = true;
        }
        else if( fr[1].matchWord( "TRANSPARENT_BIN" ) )
        {
            stateset.setRenderingHint( StateSet::TRANSPARENT_BIN );
            fr += 2;
            iteratorAdvanced = true;
        }
        else if( fr[1].isInt() )
        {
            int value;
            fr[1].getInt( value );
            stateset.setRenderingHint( value );
            fr += 2;
            iteratorAdvanced = true;
        }
    }

    bool setRenderBinDetails = false;
    StateSet::RenderBinMode rbmode = stateset.getRenderBinMode();
    if( fr[0].matchWord( "renderBinMode" ) && VEAttribute_matchRenderBinModeStr( fr[1].getStr(), rbmode ) )
    {
        setRenderBinDetails = true;
        fr += 2;
        iteratorAdvanced = true;
    }

    int binNumber = stateset.getBinNumber();
    if( fr[0].matchWord( "binNumber" ) && fr[1].getInt( binNumber ) )
    {
        setRenderBinDetails = true;
        fr += 2;
        iteratorAdvanced = true;
    }

    std::string binName = stateset.getBinName();
    if( fr[0].matchWord( "binName" ) )
    {
        setRenderBinDetails = true;
        binName = fr[1].getStr();

        fr += 2;
        iteratorAdvanced = true;
    }

    if( setRenderBinDetails )
    {
        stateset.setRenderBinDetails( binNumber, binName, rbmode );
    }


    bool readingMode = true;
    StateAttribute::GLModeValue value;
    while( readingMode )
    {

        readingMode = false;
        if( fr[0].isInt() )
        {
            if( VEAttribute_matchModeStr( fr[1].getStr(), value ) )
            {
                int mode;
                fr[0].getInt( mode );

                if( ve_TextureGLModeSet.find( mode ) != ve_TextureGLModeSet.end() )
                {
                    // remap to a texture unit.
                    stateset.setTextureMode( 0, ( StateAttribute::GLMode )mode, value );
                }
                else
                {
                    stateset.setMode(( StateAttribute::GLMode )mode, value );
                }
                fr += 2;
                iteratorAdvanced = true;
                readingMode = true;
            }
        }
        else
            if( fr[0].getStr() )
            {
                if( VEAttribute_matchModeStr( fr[1].getStr(), value ) )
                {
                    VEGLNameToGLModeMap::iterator nitr = ve_GLNameToGLModeMap.find( fr[0].getStr() );
                    if( nitr != ve_GLNameToGLModeMap.end() )
                    {
                        StateAttribute::GLMode mode = nitr->second;
                        if( ve_TextureGLModeSet.find( mode ) != ve_TextureGLModeSet.end() )
                        {
                            // remap to a texture unit.
                            stateset.setTextureMode( 0, mode, value );
                        }
                        else
                        {
                            stateset.setMode( mode, value );
                        }
                        fr += 2;
                        iteratorAdvanced = true;
                        readingMode = true;
                    }
                }
            }
    }

    // new code using osg::Registry's list of prototypes to loaded attributes.
    osg::Uniform* uniform = NULL;
    while (( uniform = fr.readUniform() ) != NULL )
    {
        stateset.addUniform( uniform );
        iteratorAdvanced = true;
    }


    // new code using osg::Registry's list of prototypes to loaded attributes.
    StateAttribute* attribute = NULL;
    while (( attribute = fr.readStateAttribute() ) != NULL )
    {
        if( attribute->isTextureAttribute() )
        {
            // remap to be a texture attribute
            stateset.setTextureAttribute( 0, attribute );
        }
        else
        {
            stateset.setAttribute( attribute );
        }
        iteratorAdvanced = true;
    }

    while( fr.matchSequence( "textureUnit %i {" ) )
    {
        int entry = fr[0].getNoNestedBrackets();

        unsigned int unit = 0;
        fr[1].getUInt( unit );
        fr += 3;

        while( !fr.eof() && fr[0].getNoNestedBrackets() > entry )
        {
            bool localIteratorAdvanced = false;

            bool readingMode = true;
            StateAttribute::GLModeValue value;
            while( readingMode )
            {
                readingMode = false;
                if( fr[0].isInt() )
                {
                    if( VEAttribute_matchModeStr( fr[1].getStr(), value ) )
                    {
                        int mode;
                        fr[0].getInt( mode );
                        stateset.setTextureMode( unit, ( StateAttribute::GLMode )mode, value );
                        fr += 2;
                        localIteratorAdvanced = true;
                        readingMode = true;
                    }
                }
                else
                    if( fr[0].getStr() )
                    {
                        if( VEAttribute_matchModeStr( fr[1].getStr(), value ) )
                        {
                            VEGLNameToGLModeMap::iterator nitr = ve_GLNameToGLModeMap.find( fr[0].getStr() );
                            if( nitr != ve_GLNameToGLModeMap.end() )
                            {
                                StateAttribute::GLMode mode = nitr->second;
                                stateset.setTextureMode( unit, mode, value );
                                fr += 2;
                                localIteratorAdvanced = true;
                                readingMode = true;
                            }
                        }
                    }
            }

            StateAttribute* attribute = NULL;
            while (( attribute = fr.readStateAttribute() ) != NULL )
            {
                stateset.setTextureAttribute( unit, attribute );
                localIteratorAdvanced = true;
            }

            if( !localIteratorAdvanced )
                fr.advanceOverCurrentFieldOrBlock();
        }

        // skip over trailing '}'
        ++fr;

        iteratorAdvanced = true;

    }




    return iteratorAdvanced;
}

// visual studio 6.0 doesn't appear to define std::max?!? So do our own here..
template<class T>
T vemymax( const T& a, const T& b )
{
    return ((( a ) > ( b ) ) ? ( a ) : ( b ) );
}

bool VEAttribute_writeLocalData( const Object& obj, Output& fw )
{

    const Attribute& stateset = static_cast<const Attribute&>( obj );

    ve_initGLNames();

    // write the rendering hint value.
    fw.indent() << "rendering_hint ";
    switch ( stateset.getRenderingHint() )
    {
        case( StateSet::DEFAULT_BIN ):
                        fw << "DEFAULT_BIN" << std::endl;
            break;
        case( StateSet::OPAQUE_BIN ):
                        fw << "OPAQUE_BIN" << std::endl;
            break;
        case( StateSet::TRANSPARENT_BIN ):
                        fw << "TRANSPARENT_BIN" << std::endl;
            break;
        default:
            fw << stateset.getRenderingHint() << std::endl;
            break;
    }

    fw.indent() << "renderBinMode " << VEAttribute_getRenderBinModeStr( stateset.getRenderBinMode() ) << std::endl;
    if( stateset.getRenderBinMode() != StateSet::INHERIT_RENDERBIN_DETAILS )
{
        fw.indent() << "binNumber " << stateset.getBinNumber() << std::endl;
        fw.indent() << "binName " << stateset.getBinName() << std::endl;
    }


    const StateSet::ModeList& ml = stateset.getModeList();
    for( StateSet::ModeList::const_iterator mitr = ml.begin();
            mitr != ml.end();
            ++mitr )
    {
        VEGLModeToGLNameMap::iterator nitr = ve_GLModeToGLNameMap.find( mitr->first );
        if( nitr != ve_GLModeToGLNameMap.end() )
        {
            fw.indent() << nitr->second << " " << VEAttribute_getModeStr( mitr->second ) << std::endl;
        }
        else
        {
            // no name defined for GLMode so just pass its value to fw.
            fw.indent() << "0x" << hex << ( unsigned int )mitr->first << dec << " " << VEAttribute_getModeStr( mitr->second ) << std::endl;
        }
    }

    const StateSet::UniformList& ul = stateset.getUniformList();
    for( StateSet::UniformList::const_iterator uitr = ul.begin();
            uitr != ul.end();
            ++uitr )
    {
        fw.writeObject( *( uitr->second.first ) );
    }

    const StateSet::AttributeList& sl = stateset.getAttributeList();
    for( StateSet::AttributeList::const_iterator sitr = sl.begin();
            sitr != sl.end();
            ++sitr )
    {
        fw.writeObject( *( sitr->second.first ) );
    }


    const StateSet::TextureModeList& tml = stateset.getTextureModeList();
    const StateSet::TextureAttributeList& tal = stateset.getTextureAttributeList();
    unsigned int maxUnit = vemymax( tml.size(), tal.size() );
    for( unsigned int unit = 0;unit < maxUnit;++unit )
    {
        fw.indent() << "textureUnit " << unit << " {" << std::endl;
        fw.moveIn();

        if( unit < tml.size() )
        {
            const StateSet::ModeList& ml = tml[unit];
            for( StateSet::ModeList::const_iterator mitr = ml.begin();
                    mitr != ml.end();
                    ++mitr )
            {
                VEGLModeToGLNameMap::iterator nitr = ve_GLModeToGLNameMap.find( mitr->first );
                if( nitr != ve_GLModeToGLNameMap.end() )
                {
                    fw.indent() << nitr->second << " " << VEAttribute_getModeStr( mitr->second ) << std::endl;
                }
                else
                {
                    // no name defined for GLMode so just pass its value to fw.
                    fw.indent() << "0x" << hex << ( unsigned int )mitr->first << dec << " " << VEAttribute_getModeStr( mitr->second ) << std::endl;
                }
            }
        }

        if( unit < tal.size() )
        {
            const StateSet::AttributeList& sl = tal[unit];
            for( StateSet::AttributeList::const_iterator sitr = sl.begin();
                    sitr != sl.end();
                    ++sitr )
            {
                fw.writeObject( *( sitr->second.first ) );
            }
        }

        fw.moveOut();
        fw.indent() << "}" << std::endl;
    }

    return true;
}


bool VEAttribute_matchModeStr( const char* str, StateAttribute::GLModeValue& mode )
{
    if( strcmp( str, "INHERIT" ) == 0 ) mode = StateAttribute::INHERIT;
    else if( strcmp( str, "ON" ) == 0 ) mode = StateAttribute::ON;
    else if( strcmp( str, "OFF" ) == 0 ) mode = StateAttribute::OFF;
    else if( strcmp( str, "OVERRIDE_ON" ) == 0 ) mode = StateAttribute::OVERRIDE | StateAttribute::ON;
    else if( strcmp( str, "OVERRIDE_OFF" ) == 0 ) mode = StateAttribute::OVERRIDE | StateAttribute::OFF;
    else if( strcmp( str, "OVERRIDE|ON" ) == 0 ) mode = StateAttribute::OVERRIDE | StateAttribute::ON;
    else if( strcmp( str, "OVERRIDE|OFF" ) == 0 ) mode = StateAttribute::OVERRIDE | StateAttribute::OFF;
    else if( strcmp( str, "PROTECTED|ON" ) == 0 ) mode = StateAttribute::PROTECTED | StateAttribute::ON;
    else if( strcmp( str, "PROTECTED|OFF" ) == 0 ) mode = StateAttribute::PROTECTED | StateAttribute::OFF;
    else if( strcmp( str, "PROTECTED|OVERRIDE|ON" ) == 0 ) mode = StateAttribute::PROTECTED | StateAttribute::OVERRIDE | StateAttribute::ON;
    else if( strcmp( str, "PROTECTED|OVERRIDE|OFF" ) == 0 ) mode = StateAttribute::PROTECTED | StateAttribute::OVERRIDE | StateAttribute::OFF;
    else return false;
    return true;
}


const char* VEAttribute_getModeStr( StateAttribute::GLModeValue value )
{
    switch ( value )
    {
        case( StateAttribute::INHERIT ): return "INHERIT";
        case( StateAttribute::ON ): return "ON";
        case( StateAttribute::OFF ): return "OFF";
        case( StateAttribute::OVERRIDE | StateAttribute::ON ): return "OVERRIDE|ON";
        case( StateAttribute::OVERRIDE | StateAttribute::OFF ): return "OVERRIDE|OFF";
        case( StateAttribute::PROTECTED | StateAttribute::ON ): return "PROTECTED|ON";
        case( StateAttribute::PROTECTED | StateAttribute::OFF ): return "PROTECTED|OFF";
        case( StateAttribute::PROTECTED | StateAttribute::OVERRIDE | StateAttribute::ON ): return "PROTECTED|OVERRIDE|ON";
        case( StateAttribute::PROTECTED | StateAttribute::OVERRIDE | StateAttribute::OFF ): return "PROTECTED|OVERRIDE|OFF";
    }
    return "";
}

bool VEAttribute_matchRenderBinModeStr( const char* str, StateSet::RenderBinMode& mode )
{
    if( strcmp( str, "INHERIT" ) == 0 ) mode = StateSet::INHERIT_RENDERBIN_DETAILS;
    else if( strcmp( str, "USE" ) == 0 ) mode = StateSet::USE_RENDERBIN_DETAILS;
    else if( strcmp( str, "OVERRIDE" ) == 0 ) mode = StateSet::OVERRIDE_RENDERBIN_DETAILS;
    else if( strcmp( str, "ENCLOSE" ) == 0 ) mode = StateSet::USE_RENDERBIN_DETAILS;
    else return false;
    return true;
}

const char* VEAttribute_getRenderBinModeStr( StateSet::RenderBinMode mode )
{
    switch ( mode )
    {
        case( StateSet::INHERIT_RENDERBIN_DETAILS ):  return "INHERIT";
        case( StateSet::USE_RENDERBIN_DETAILS ):      return "USE";
        case( StateSet::OVERRIDE_RENDERBIN_DETAILS ): return "OVERRIDE";
    }
    return "";
}
