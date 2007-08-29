#ifndef PERLIN_NOISE_TEXTURE_H
#define PERLIN_NOISE_TEXTURE_H

#include <osg/Texture>
#include <osg/Image>
namespace VE_SceneGraph
{
namespace Utilities
{
class PerlinNoiseTexture
{
public:
    ///Constructor
    ///\param xdim The texture size s dimension
    ///\param xdim The texture size t dimension
    ///\param xdim The texture size r dimension
    PerlinNoiseTexture(unsigned int sdim = 128,
                            unsigned int tdim = 128,
                            unsigned int rdim = 128);
    ///Destructor
    virtual ~PerlinNoiseTexture();

    ///Get the noise texture
    osg::Texture* GetNoiseTexture();
protected:
    ///Initialize the noise texture
   ///\param s The texture size s dimension
    ///\param t The texture size t dimension
    ///\param r The texture size r dimension
    void _initNoiseTexture(int s, int t, int r );

    ///Initialize the noise image data
    ///\param s The image size s dimension
    ///\param t The image size t dimension
    ///\param r The image size r dimension
    void _initNoiseImage(int s, int t, int r );

    osg::ref_ptr<osg::Texture> m_noiseTexture;///<The perlin noise texture.
    osg::ref_ptr<osg::Image> m_noiseImage;///<The perlin noise data.
};
}
}
#endif //PERLIN_NOISE_TEXTURE_H
