#ifndef CAMERA_CALLBACK_H
#define CAMERA_CALLBACK_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/Camera>

// --- STL Includes --- //
#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

struct VE_SCENEGRAPH_EXPORTS CameraImageCaptureCallback :
    public osg::Camera::DrawCallback
{
public:
    CameraImageCaptureCallback( const std::string& filename, int w, int h );

    virtual void operator()( osg::RenderInfo& ri ) const;

protected:
    std::string m_filename;

    int w_;
    int h_;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //CAMERA_CALLBACK_H
