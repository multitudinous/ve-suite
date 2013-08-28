#include <ves/conductor/qt/TextureSubloader.h>

#include <iostream>

namespace ves
{
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
TextureSubloader::TextureSubloader()
    :
    doSubload( false ),
    m_enabled( true )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TextureSubloader::~TextureSubloader()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TextureSubloader::SetEnabled( bool enabled )
{
    m_enabled = enabled;
}
////////////////////////////////////////////////////////////////////////////////
// create the OpenGL texture. A necessary override of osg::Texture2D::SubloadCallback (overrides a pure virtual).
void TextureSubloader::load( const osg::Texture2D& texture, osg::State& ) const
{
    if( !m_enabled )
    {
        return;
    }

    const osg::Image* image = texture.getImage();
    if( image )
    {
        m_textureWidth = image->s();
        m_textureHeight = image->t();
        // texture must have an image to work with.
        glTexImage2D( GL_TEXTURE_2D, 0, image->getPixelFormat(), m_textureWidth, m_textureHeight, 0, image->getPixelFormat(), image->getDataType(), image->data() );
    }
}
////////////////////////////////////////////////////////////////////////////////
// overlay the image onto the texture. A necessary override of osg::Texture2D::SubloadCallback (overrides a pure virtual).
void TextureSubloader::subload( const osg::Texture2D& texture, osg::State& ) const
{
    if( !m_enabled )
    {
        return;
    }

    if( subImgs.empty() )
    {
        return;
    }

    /*const unsigned int contextID = state.getContextID();
    if( _contexts.find( contextID ) != _contexts.end() )
    {
        return;
    }
    _contexts.insert( contextID );*/

    // Test whether texture size has changed since last subload. If so,
    // re-bind the new texture image.
    const osg::Image* image = texture.getImage();
    if( image )
    {
        if( (image->s() != m_textureWidth ) || ( image->t() != m_textureHeight ) )
        {
            m_textureWidth = image->s();
            m_textureHeight = image->t();
            glTexImage2D( GL_TEXTURE_2D, 0, image->getPixelFormat(), m_textureWidth, m_textureHeight, 0, image->getPixelFormat(), image->getDataType(), image->data() );
        }
    }

    osg::ref_ptr< osg::Image > subImg;
    int xOffset = 0;
    int yOffset = 0;
    for( size_t c = 0; c < subImgs.size(); ++c )
    {
        subImg = subImgs.at( c );
        xOffset = xOffsets.at( c );
        yOffset = yOffsets.at( c );
        // copy the image into the current texture (the one currently bound / selected) at the given offsets.
        glEnable( GL_TEXTURE_2D );       // make sure 2D textures are enabled.
        glPixelStorei( GL_UNPACK_ALIGNMENT, subImg->getPacking() );    // image unpacking.
        // it probably doesn't make sense to subload to a mipmap, but in any event, select the nearest one and operate on that.
        // in the case of a mipmapped texture, it is especially important to set these filters, or the result won't be shown.
        // a texture that doesn't have defined filters is treated as mipmapped.
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
        // copy the image into place.
        glTexSubImage2D( GL_TEXTURE_2D, 0, xOffset, yOffset, subImg->s(),
                         subImg->t(), subImg->getPixelFormat(),
                         subImg->getDataType(), subImg->data() );
        subImg = 0;                      // don't hold on to the image pointer any longer.
    }

    // completed the subload. Need another call to AddUpdate() before the next one.
    doSubload = false;
}
////////////////////////////////////////////////////////////////////////////////
// tell the next subload callback to copy the input image to the specified offsets
// in the texture for this.
void TextureSubloader::AddUpdate( osg::Image* img, int xOffset, int yOffset )
{
    if( !doSubload )
    {
        ClearData();
    }

    if( img != 0 )
    {
        subImgs.push_back( img );
        xOffsets.push_back( xOffset );
        yOffsets.push_back( yOffset );
        doSubload = true;
    }
}
////////////////////////////////////////////////////////////////////////////////
void TextureSubloader::ClearData()
{
    subImgs.clear();
    xOffsets.clear();
    yOffsets.clear();
}
////////////////////////////////////////////////////////////////////////////////
}
}
