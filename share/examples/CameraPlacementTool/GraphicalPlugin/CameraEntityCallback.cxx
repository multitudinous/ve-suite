// --- My Includes --- //
#include "CameraEntityCallback.h"
#include "CameraEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>

// --- OSG Includes --- //
#include <osg/TexGenNode>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::CameraEntityCallback()
:
osg::Object(),
osg::NodeCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::CameraEntityCallback( const CameraEntityCallback& input )
:
osg::Object( input ),
osg::NodeCallback( input )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::~CameraEntityCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::ref_ptr< cpt::CameraEntity > cameraEntity =
        static_cast< cpt::CameraEntity* >( node );

    if( cameraEntity.valid() )
    {
        osg::Matrixd dcsInverseMatrix;
        gmtl::Matrix44d temp = cameraEntity->GetDCS()->GetMat();
        dcsInverseMatrix.set( gmtl::invert( temp ).getData() );

        //Compute matrix that takes a vertex from local coords into tex coords
        osg::Matrixd MVPT = dcsInverseMatrix * cameraEntity->GetMatrixMVPT();
        cameraEntity->GetTexGenNode()->getTexGen()->setPlanesFromMatrix( MVPT );
    }

    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
