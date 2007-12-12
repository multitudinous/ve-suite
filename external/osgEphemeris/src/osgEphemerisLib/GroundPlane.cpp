/* -*-c++-*- OpenSceneGraph - Ephemeris Model Copyright (C) 2005 Don Burns
 *
 * This library is open source and may be redistributed and/or modified under
 * the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or
 * (at your option) any later version.  The full license is in LICENSE file
 * included with this distribution, and on the openscenegraph.org website.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * OpenSceneGraph Public License for more details.
*/
#include <osgEphemeris/GroundPlane>
#include <osgDB/ReadFile>
#include <osg/Texture2D>
#include <osg/Image>
#include <osg/Program>
#include <osg/Image>
#include <osg/PolygonMode>

#include <osg/StateSet>

#include <iostream>

using namespace osgEphemeris;

GroundPlane::GroundPlane( double radius )
:m_radius(radius),
m_origin(-m_radius,-m_radius,0.0f),
m_size(2.0*m_radius,2.0*m_radius,.1*m_radius),
m_scaleDown(1.0f/m_size.x(),1.0f/m_size.y(),1.0f/m_size.z()),
m_altitudeRange(0.0,m_size.z())
{
    _initializeSimpleTerrainGeneratorShader();
    UpdateBaseTerrainFromImage("Images/lz.rgb");
    if(!m_terrainGrid.valid())
    {
        std::cout<<"Unable to create default terrain!"<<std::endl;
        osg::ref_ptr<osg::Vec3Array> coords = new osg::Vec3Array;

        coords->push_back( osg::Vec3( -radius, -radius, -1000.0 ));
        coords->push_back( osg::Vec3(  radius, -radius, -1000.0 ));
        coords->push_back( osg::Vec3(  radius,  radius, -1000.0 ));
        coords->push_back( osg::Vec3( -radius,  radius, -1000.0 ));

        osg::ref_ptr<osg::Vec3Array> normals = new osg::Vec3Array;
        normals->push_back( osg::Vec3(0,0,1));

        osg::ref_ptr<osg::Vec4Array> colors  = new osg::Vec4Array;
        colors->push_back( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ));

        osg::ref_ptr<osg::Vec2Array> tcoords = new osg::Vec2Array;
        tcoords->push_back( osg::Vec2( 0.0, 0.0 ));
        tcoords->push_back( osg::Vec2( 1.0, 0.0 ));
        tcoords->push_back( osg::Vec2( 1.0, 1.0 ));
        tcoords->push_back( osg::Vec2( 0.0, 1.0 ));

        m_terrainGrid = new osg::Geometry;
        m_terrainGrid->setVertexArray( coords.get() );
        m_terrainGrid->setTexCoordArray( 0, tcoords.get() );

        m_terrainGrid->setNormalArray( normals.get() );
        m_terrainGrid->setNormalBinding( osg::Geometry::BIND_OVERALL );

        m_terrainGrid->setColorArray( colors.get() );
        m_terrainGrid->setColorBinding( osg::Geometry::BIND_OVERALL );

        m_terrainGrid->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS,
                                                             0, coords->size() ));

    }
    addDrawable( m_terrainGrid.get() );
}
//////////////////////////////////////////////////////////////////////
bool GroundPlane::UpdateBaseTerrainFromImage(std::string terrainImage)
{
    m_baseTerrainImage = osgDB::readImageFile(terrainImage);    
    if(!m_baseTerrainImage.valid())
    {
        std::cout<<"Failed to load terrain file: "<<terrainImage<<std::endl;
        return false;    
    }
    //need to force this as the internal format so that vertex texture lookups don't fallback
    //to software on shader model 3.0 cards
    m_baseTerrainImage->setInternalTextureFormat(GL_RGBA_FLOAT32_ATI);

    std::map< std::string, osg::ref_ptr< osg::Texture2D > >::iterator foundVertTexture;
    foundVertTexture = m_terrainVertTexture.find(terrainImage);

    if( ( foundVertTexture != m_terrainVertTexture.end() ) )
    {
        if(!foundVertTexture->second->getImage())
        {
            m_terrainVertTexture[terrainImage]->setImage(m_baseTerrainImage.get());
        }
        UpdateTextureUnitsInStateSet( m_terrainPrograms["BASIC_TERRAIN_SHADER"].get(),
                                      foundVertTexture->second.get());
    }
    else
    {
        m_terrainVertTexture[terrainImage] = new osg::Texture2D();
        m_terrainVertTexture[terrainImage]->setInternalFormatMode(osg::Texture::USE_IMAGE_DATA_FORMAT);
        m_terrainVertTexture[terrainImage]->setImage(m_baseTerrainImage.get());
        m_terrainVertTexture[terrainImage]->setFilter(osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST);
        m_terrainVertTexture[terrainImage]->setFilter(osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST);
        m_terrainVertTexture[terrainImage]->setResizeNonPowerOfTwoHint(false);

        UpdateTextureUnitsInStateSet( m_terrainPrograms["BASIC_TERRAIN_SHADER"].get(),
                                      m_terrainVertTexture[terrainImage].get());
    }
    

    //Update the terrain grid from the new texture dimensions
    _updateTerrainGrid(m_baseTerrainImage->s(),m_baseTerrainImage->t());
    return true;
}
///////////////////////////////////////////////////////////////////////////////////////
void GroundPlane::_updateTerrainGrid(unsigned int imageWidth, unsigned int imageHeight)
{
    int width = 2*imageWidth;
    int height = 2*imageHeight;
    float delta[2] = {0.0,0.0};
    delta[0] = m_size.x()/(float)(width-1);
    delta[1] = m_size.y()/(float)(height-1);
    if(!m_terrainGrid.valid())
    {
        m_terrainGrid = new osg::Geometry();
    }
    else
    {
       m_terrainGrid->removePrimitiveSet(0,m_terrainGrid->getNumPrimitiveSets());
       m_terrainGrid->getVertexArray()->dirty();
    }
    m_terrainGrid->setStateSet( m_terrainPrograms["BASIC_TERRAIN_SHADER"].get());

    osg::ref_ptr<osg::Vec3Array> verts = new osg::Vec3Array();
    osg::ref_ptr<osg::Vec4Array> color = new osg::Vec4Array();
    osg::ref_ptr<osg::Vec3Array> normals = new osg::Vec3Array;
    normals->push_back( osg::Vec3(0,0,1));
    color->push_back(osg::Vec4(1.0,0.0,0.0,1.0));

    //specify vertecies
     
    for(unsigned int rows = 0; rows < height-1; ++rows)
    {
        for(unsigned int cols = 0; cols < width; ++cols)
        {
            verts->push_back(osg::Vec3(m_origin.x() + (cols)*delta[0], 
                                       m_origin.y() + (rows+1)*delta[1], 
                                       m_origin.z()));
            verts->push_back(osg::Vec3(m_origin.x() + (cols)*delta[0], 
                                       m_origin.y() + (rows)*delta[1],
                                       m_origin.z()));
        }
        m_terrainGrid->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUAD_STRIP,
                                                           rows*2*width,width*2));
    }
    m_terrainGrid->setVertexArray(verts.get());
    m_terrainGrid->setColorArray(color.get());
    m_terrainGrid->setColorBinding(osg::Geometry::BIND_OVERALL);
    m_terrainGrid->setNormalArray( normals.get() );
    m_terrainGrid->setNormalBinding( osg::Geometry::BIND_OVERALL );
    m_terrainGrid->setInitialBound(osg::BoundingBox(m_origin, m_origin + m_size));
}
///////////////////////////////////////////////////////////
void GroundPlane::_initializeSimpleTerrainGeneratorShader()
{
    //right now only one stateset
    {
        osg::ref_ptr<osg::StateSet> stateset = new osg::StateSet();
        osg::ref_ptr<osg::Uniform> originUniform =
                           new osg::Uniform("terrainOrigin",m_origin);

        stateset->addUniform(originUniform.get());

        osg::ref_ptr<osg::Uniform> sizeUniform =
                                     new osg::Uniform("terrainSize",m_size);
        stateset->addUniform(sizeUniform.get());

        osg::ref_ptr<osg::Uniform> scaleDownUniform =
                                    new osg::Uniform("terrainScaleDown",m_scaleDown);
        stateset->addUniform(scaleDownUniform.get());

        stateset->addUniform(new osg::Uniform("altitudeRange",m_altitudeRange));

        m_terrainVertTexture["Images/lz.rgb"] = new osg::Texture2D();
        osg::ref_ptr<osg::Uniform> baseVertTextureSampler =
                                    new osg::Uniform("baseVertTexture",0);
        m_terrainVertTexture["Images/lz.rgb"]->setFilter(osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST);
        m_terrainVertTexture["Images/lz.rgb"]->setFilter(osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST);

        stateset->addUniform(baseVertTextureSampler.get());

        // vertex shader using just Vec4 coefficients
        char vertexShaderSource[] =
               
               "uniform sampler2D baseVertTexture; \n"
               "uniform vec3 terrainOrigin;\n"
               "uniform vec3 terrainScaleDown;\n"
               "uniform vec2 altitudeRange;\n"
               "\n"
               "varying vec4 texColor;\n"
               "\n"
               "float greyscale(vec3 rgbColor)\n"
               "{\n"
               "    return dot(vec3(0.2125,0.7154,0.0721),rgbColor);\n"
               "}\n"
               "void main(void)\n"
               "{\n"
               "    vec2 texcoord = gl_Vertex.xy - terrainOrigin.xy;\n"
               "    texcoord.x *= terrainScaleDown.x;\n"
               "    texcoord.y *= terrainScaleDown.y;\n"
               "\n"
               "    vec4 position;\n"
               "    position.x = gl_Vertex.x;\n"
               "    position.y = gl_Vertex.y;\n"
               "    texColor = texture2D(baseVertTexture, texcoord);\n"
               "    position.z = mix(altitudeRange.x,altitudeRange.y,greyscale(texColor.rgb));\n"
               "    position.w = 1.0;\n"
               " \n"
               "    gl_Position     = gl_ModelViewProjectionMatrix * position;\n"
                "   gl_FrontColor = vec4(1.0,1.0,1.0,1.0);\n"
               "}\n";

            //////////////////////////////////////////////////////////////////
            // fragment shader
            //
            char fragmentShaderSource[] =
                "varying vec4 texColor;\n"
                "\n"
                "void main(void) \n"
                "{\n"
                "    gl_FragColor = texColor; \n"
                "}\n";

        osg::ref_ptr<osg::Program> program = new osg::Program();
        program->addShader(new osg::Shader(osg::Shader::VERTEX, vertexShaderSource));
        program->addShader(new osg::Shader(osg::Shader::FRAGMENT, fragmentShaderSource)); 
        stateset->setAttribute(program.get());
        m_terrainPrograms["BASIC_TERRAIN_SHADER"] = stateset;
        UpdateTextureUnitsInStateSet( m_terrainPrograms["BASIC_TERRAIN_SHADER"].get(),
                                      m_terrainVertTexture["Images/lz.rgb"].get());
    }    
}
///////////////////////////////////////////////////////////////////////////
void GroundPlane::UpdateTextureUnitsInStateSet( osg::StateSet* ss,
                                                osg::Texture2D* vertTexture)
{
    ss->removeTextureAttribute( 0, osg::StateAttribute::TEXTURE );
    ss->setTextureAttributeAndModes( 0,
                                     vertTexture,
                                     osg::StateAttribute::ON);
}
////////////////////////////////////////////////////////////////////////
void GroundPlane::SetAltitudeRange(float minAltitude, float maxAltitude)
{
    //probably could tie this to a Uniform update callback
    m_altitudeRange.set(minAltitude,maxAltitude);
    std::map<std::string, osg::ref_ptr<osg::StateSet> >::iterator currentStateSet;
    for(currentStateSet = m_terrainPrograms.begin();
        currentStateSet != m_terrainPrograms.end();
        ++currentStateSet)
    {  
        if(currentStateSet->second->getUniform("altitudeRange"))
        {
            currentStateSet->second->getUniform("altitudeRange")
                            ->set(osg::Vec2(minAltitude,maxAltitude));
        }
    }
}
