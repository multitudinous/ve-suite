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
#include <map>
#include <string>

namespace hyperlab
{
class Shaders
{
public:
    Shaders();
    ~Shaders();

    void ReadTextures();

    void SetOptions( osg::ref_ptr< osg::Node > node,
                     bool xray = false,
                     bool phong = false,
                     const std::string baseMap = "",
                     float* reflectionPercent = NULL,
                     osg::ref_ptr< osg::Texture2D > shadow = NULL );

    std::map< std::string, osg::ref_ptr< osg::Image > > m_imageMap;
    osg::ref_ptr< osg::TextureCubeMap > m_tcm;
};
} //end hyperlab

#endif //SHADERS_H
