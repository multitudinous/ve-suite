#ifndef CAMERA_CALLBACK_H
#define CAMERA_CALLBACK_H

#include <osgDB/WriteFile>
#include <osg/Node>
#include <osg/Camera>
#include <osg/MatrixTransform>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/Image>
#include <osg/Depth>
#include <osg/BlendColor>
#include <osg/BlendFunc>
#include <osgDB/FileUtils>
#include <osg/io_utils>

#include <ves/VEConfig.h>

struct VE_SCENEGRAPH_EXPORTS CameraImageCaptureCallback : public osg::Camera::DrawCallback
{
public:
    CameraImageCaptureCallback( const std::string& filename, int w, int h )
    : filename( filename ),
    w_( w ),
    h_( h )
    {}

    virtual void operator()( osg::RenderInfo& ri ) const
    {
        osg::Image* image = new osg::Image;
        std::string fName( filename + std::string( ".png" ) );

        osg::notify( osg::ALWAYS ) << "Reading image for file " << fName << " ... " << std::endl;
        image->readPixels( 0, 0, w_, h_, GL_RGBA, GL_UNSIGNED_BYTE );

        osg::notify( osg::ALWAYS ) << "  Writing file " << fName << " ... " << std::endl;
        osgDB::writeImageFile( *image, fName );

        osg::notify( osg::ALWAYS ) << "  Capture complete." << std::endl;
    }

protected:
    std::string filename;
    int w_, h_;
};

#endif
