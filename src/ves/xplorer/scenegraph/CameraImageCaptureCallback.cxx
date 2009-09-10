
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CameraImageCaptureCallback.h>

// --- OSG Includes --- //
#include <osg/Image>
#include <osg/io_utils>

#include <osgDB/WriteFile>
#include <osgDB/FileUtils>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CameraImageCaptureCallback::CameraImageCaptureCallback(
    const std::string& filename, int w, int h )
    :
    m_filename( filename ),
    w_( w ),
    h_( h )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraImageCaptureCallback::operator()( osg::RenderInfo& ri ) const
{
    osg::Image* image = new osg::Image();
    std::string fName( m_filename + std::string( ".png" ) );

    osg::notify( osg::ALWAYS ) << "Reading image for file " << fName << " ... " << std::endl;
    image->readPixels( 0, 0, w_, h_, GL_RGBA, GL_UNSIGNED_BYTE );

    osg::notify( osg::ALWAYS ) << "  Writing file " << fName << " ... " << std::endl;
    osgDB::writeImageFile( *image, fName );

    osg::notify( osg::ALWAYS ) << "  Capture complete." << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
