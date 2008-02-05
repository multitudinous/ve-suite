#ifndef SHADERS_H
#define SHADERS_H

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
    class Node;
    class Image;
    class StateSet;
    class Texture2D;
    class TextureCubeMap;
}

// --- C/C++ Libraries
#include <vector>

typedef std::vector< osg::ref_ptr< osg::Image > > ImageList;

class Shaders
{
public:
    Shaders();
    ~Shaders();

    void ReadTextures();

    void Base( osg::ref_ptr< osg::Node > node );

    void Phong( osg::ref_ptr< osg::Node > node );
    void Texture( int tex_num, osg::ref_ptr< osg::Node > node );
    void PCF( osg::ref_ptr< osg::Texture2D > shadow, osg::ref_ptr< osg::Node > node );
    void Reflection( float refl_perc, osg::ref_ptr< osg::Node > node );
    void XRay( osg::ref_ptr< osg::Node > node );

    void Phong_Texture( int tex_num, osg::ref_ptr< osg::Node > node );
    void Phong_PCF( osg::ref_ptr< osg::Texture2D > shadow, osg::ref_ptr< osg::Node > node );
    void Phong_Reflection( float refl_perc, osg::ref_ptr< osg::Node > node );
    void Texture_PCF( int tex_num, osg::ref_ptr< osg::Texture2D > shadow, osg::ref_ptr< osg::Node > node);
    void Texture_Reflection( int tex_num, float refl_perc, osg::ref_ptr< osg::Node > node);
    void PCF_Reflection( float refl_perc, osg::ref_ptr< osg::Texture2D > shadow, osg::ref_ptr< osg::Node > node );

    void Phong_Texture_PCF( int tex_num, osg::ref_ptr< osg::Texture2D > shadow, osg::ref_ptr< osg::Node > node );
    void Phong_Texture_Reflection( int tex_num, float refl_perc, osg::ref_ptr< osg::Node > node );
    void Phong_PCF_Reflection( float refl_perc, osg::ref_ptr< osg::Texture2D > shadow, osg::ref_ptr<osg::Node> node );
    void Texture_PCF_Reflection( int tex_num, float refl_perc, osg::ref_ptr< osg::Texture2D > shadow, osg::ref_ptr< osg::Node > node );

    void Phong_Texture_PCF_Reflection( int tex_num, float refl_perc, osg::ref_ptr< osg::Texture2D > shadow, osg::ref_ptr< osg::Node > node );

    ImageList imageList;
    osg::ref_ptr< osg::TextureCubeMap > tcm;
};

#endif //SHADERS_H
