#ifndef CAMERA_ENTITY_CALLBACK_H
#define CAMERA_ENTITY_CALLBACK_H

// --- VE-Suite Includes --- //
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
}
}
}

// --- OSG Includes --- //
#include <osg/NodeCallback>
#include <osg/Matrix>

namespace osg
{
    class TexGenNode;
}

namespace cpt
{
/*----------------------------------------------------------------------------*/
class CameraEntityCallback : public osg::NodeCallback
{
public:

    CameraEntityCallback();
    CameraEntityCallback( const CameraEntityCallback& input );

    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

    void SetDCS( ves::xplorer::scenegraph::DCS* dcs );
    void SetTexGenNode( osg::TexGenNode* texGenNode );
    void SetMatrixMVPT( const osg::Matrixd& MVPT );

protected:
    virtual ~CameraEntityCallback();

private:
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mDcs;
    osg::ref_ptr< osg::TexGenNode > mTexGenNode;

    osg::Matrixd mMVPT;

};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_ENTITY_CALLBACK_H
