#ifndef CAMERA_ENTITY_CALLBACK_H
#define CAMERA_ENTITY_CALLBACK_H

// --- VE-Suite Includes --- //

// --- OSG Includes --- //
#include <osg/NodeCallback>

namespace cpt
{
/*----------------------------------------------------------------------------*/
class CameraEntityCallback : public osg::NodeCallback
{
public:

    CameraEntityCallback();
    CameraEntityCallback( const CameraEntityCallback& input );

    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

protected:
    virtual ~CameraEntityCallback();

private:

};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_ENTITY_CALLBACK_H
