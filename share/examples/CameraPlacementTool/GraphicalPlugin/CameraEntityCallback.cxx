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
void CameraEntityCallback::SetMatrixMVPT( const osg::Matrixd& MVPT )
{
    m_MVPT = MVPT;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntityCallback::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    //osg::ref_ptr< osg::Camera > camera = static_cast< osg::Camera* >( node );

    if( m_dcs.valid() )
    {
        osg::Matrixd dcsInverseMatrix;
        gmtl::Matrix44d tempDCSMat = m_dcs->GetMat();
        dcsInverseMatrix.set( gmtl::invert( tempDCSMat ).getData() );

        //Compute the matrix which takes a vertex from local coords into tex coords
        osg::Matrixd MVPT = dcsInverseMatrix * m_MVPT;
        m_texGenNode->getTexGen()->setPlanesFromMatrix( MVPT );

        //Need to update CameraEntity's MVPT value somehow
    }

    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
