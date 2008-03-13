// --- My Includes --- //
#include "CameraEntityCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- vrJuggler Includes --- //
#include <gmtl/Xforms.h>

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/TexGenNode>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::CameraEntityCallback()
:
m_dcs( 0 ),
m_texGenNode( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::CameraEntityCallback( const CameraEntityCallback& input )
:
osg::Object( input ),
osg::NodeCallback( input ),
m_dcs( 0 ),
m_texGenNode( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraEntityCallback::~CameraEntityCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::SetDCS( ves::xplorer::scenegraph::DCS* dcs )
{
    m_dcs = dcs;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::SetTexGenNode( osg::TexGenNode* texGenNode )
{
    m_texGenNode = texGenNode;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::SetMatrixPT( const osg::Matrixd& PT )
{
    m_PT = PT;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::ref_ptr< osg::Camera > camera = static_cast< osg::Camera* >( node );

    if( camera.valid() )
    {
        osg::Matrixd dcsMatrix;
        dcsMatrix.set( gmtl::invert( m_dcs->GetMat() ).getData() );
        osg::Matrixd modelViewMatrix = dcsMatrix * camera->getViewMatrix();
        //Compute the matrix which takes a vertex from local coords into tex coords
        m_texGenNode->getTexGen()->setPlanesFromMatrix( modelViewMatrix * m_PT );
    }

    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
