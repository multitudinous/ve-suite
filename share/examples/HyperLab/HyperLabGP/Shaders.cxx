// --- My Includes --- //
#include "VertFrag.h"
#include "Shaders.h"

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/StateSet>
#include <osg/Texture2D>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

// --- C/C++ Libraries --- //

using namespace hyperlab;

////////////////////////////////////////////////////////////////////////////////
Shaders::Shaders()
{
    ReadTextures();
}
////////////////////////////////////////////////////////////////////////////////
Shaders::~Shaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::ReadTextures()
{
    m_imageMap.insert( std::make_pair( "Decoration", osgDB::readImageFile( "./Textures/Decoration.tga" ) ) );
    m_imageMap.insert( std::make_pair( "WallMap", osgDB::readImageFile( "./Textures/WallMap.tga" ) ) );
    m_imageMap.insert( std::make_pair( "Corona", osgDB::readImageFile( "./Textures/Corona.tga" ) ) );

    m_tcm = new osg::TextureCubeMap();
    m_tcm->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setWrap( osg::Texture::WRAP_R, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setFilter( osg::Texture::MIN_FILTER, osg::Texture::LINEAR );

    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_X, osgDB::readImageFile( "./Textures/CubeMap/Right.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_X, osgDB::readImageFile( "./Textures/CubeMap/Left.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_Y, osgDB::readImageFile( "./Textures/CubeMap/Top.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_Y, osgDB::readImageFile( "./Textures/CubeMap/Bottom.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_Z, osgDB::readImageFile( "./Textures/CubeMap/Back.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_Z, osgDB::readImageFile( "./Textures/CubeMap/Front.tga" ) );
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::SetOptions( osg::ref_ptr< osg::Node > node,
                          bool xray,
                          bool phong,
                          const std::string baseMap,
                          float* reflectionPercent,
                          osg::ref_ptr< osg::Texture2D > shadow )
{
    osg::ref_ptr< osg::StateSet > stateset = node->getOrCreateStateSet();
    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    stateset->setAttribute( program.get() );

    std::map< std::string, bool > optionsMap;
    optionsMap.insert( std::make_pair( "phong", false ) );
    optionsMap.insert( std::make_pair( "baseMap", false ) );
    optionsMap.insert( std::make_pair( "envMap", false ) );
    optionsMap.insert( std::make_pair( "shadowMap", false ) );

    if( xray )
    {
        stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
        stateset->setRenderingHint( osg::StateSet::TRANSPARENT_BIN );

        vertexShader->setShaderSource( xray_vertex );
        fragmentShader->setShaderSource( xray_fragment );

        return;
    }

    if( phong )
    {
        optionsMap[ "phong" ] = true;
    }

    if( !baseMap.empty() )
    {
        stateset->setTextureAttributeAndModes( 2, new osg::Texture2D( m_imageMap[ baseMap ].get() ) );

        osg::ref_ptr< osg::Uniform > baseMapUniform = new osg::Uniform( "baseMap", 2 );
        stateset->addUniform( baseMapUniform.get() );

        optionsMap[ "baseMap" ] = true;
    }

    if( reflectionPercent )
    {
        stateset->setTextureAttributeAndModes( 1, m_tcm.get(), osg::StateAttribute::ON );

        //osg::ref_ptr< osg::Uniform > reflection_percentage = new osg::Uniform( "refl_perc", *reflectionPercent );
        //stateset->addUniform( reflection_percentage.get() );
        osg::ref_ptr< osg::Uniform > envMap = new osg::Uniform( "envMap", 1 );
        stateset->addUniform( envMap.get() );

        optionsMap[ "envMap" ] = true;
    }

    if( shadow.valid() )
    {
        stateset->setTextureAttributeAndModes( 0, shadow.get(), osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_R, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );

        osg::ref_ptr< osg::Uniform > shadowMap = new osg::Uniform( "shadowMap", 0 );
        stateset->addUniform( shadowMap.get() );

        optionsMap[ "shadowMap" ] = true;
    }

    osg::ref_ptr< osg::Uniform > options = new osg::Uniform( "options", optionsMap[ "phong" ],
                                                                        optionsMap[ "baseMap" ],
                                                                        optionsMap[ "envMap" ],
                                                                        optionsMap[ "shadowMap" ] );
    stateset->addUniform( options.get() );

    vertexShader->setShaderSource( options_vertex );
    fragmentShader->setShaderSource( options_fragment );
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Lights( osg::ref_ptr< osg::Node > node )
{
    osg::ref_ptr< osg::StateSet > stateset = node->getOrCreateStateSet();
    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    stateset->setAttribute( program.get() );

    stateset->setTextureAttributeAndModes( 0, new osg::Texture2D( m_imageMap[ "Corona" ].get() ) );

    osg::ref_ptr< osg::Uniform > baseMap = new osg::Uniform( "baseMap", 0 );
    stateset->addUniform( baseMap.get() );

    vertexShader->setShaderSource( lights_vertex );
    fragmentShader->setShaderSource( lights_fragment );
}
////////////////////////////////////////////////////////////////////////////////