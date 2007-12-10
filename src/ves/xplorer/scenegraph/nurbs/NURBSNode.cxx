/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <ves/xplorer/scenegraph/nurbs/NURBSNode.h>
#include <ves/xplorer/scenegraph/nurbs/NSurface.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <osg/Drawable>
#include <osg/BoundingBox>
#include <osg/BoundingSphere>
#include <osg/ShadeModel>
#include <osg/Material>
#include <osg/GLU>
#include <osg/Matrixd>
#include <osg/Version>

#include <iostream>

using namespace ves::xplorer::scenegraph::nurbs;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
/////////////////////////////////////////////////////////////
///Class for the control mesh drawable                     //
/////////////////////////////////////////////////////////////
class VE_NURBS_EXPORTS NURBSControlMesh: public osg::Drawable
{
public:
    ///Constructor
    NURBSControlMesh()
    {
        m_hasSelectedControlPoint = false;
    }

    NURBSControlMesh( std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint>* controlPoints,
                      unsigned int numU,
                      unsigned int numV, bool isSurface = false )
    {
        _controlPoints = controlPoints;
        _numUControlPoints = numU;
        _numVControlPoints = numV;
        _isSurface = isSurface;
        setUseDisplayList( false );
        _mouse[0] = 0;
        _mouse[1] = 0;
        _modelViewMatrix = 0;
        _projectionMatrix = 0;
        _selectedControlPointIndex = -1;
        m_hasSelectedControlPoint = false;
    }

    ///Copy constructor
    NURBSControlMesh( const NURBSControlMesh& controlMesh,
                      const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY ):
            osg::Drawable( controlMesh, copyop )
    {
        _controlPoints = controlMesh._controlPoints;
        _numUControlPoints = controlMesh._numUControlPoints;
        _numVControlPoints = controlMesh._numVControlPoints;
        _isSurface = controlMesh._isSurface;
        _mouse[0] = controlMesh._mouse[0];
        _mouse[1] = controlMesh._mouse[1];
        _modelViewMatrix = controlMesh._modelViewMatrix;
        _inverseModelViewMatrix = controlMesh._inverseModelViewMatrix;
        _inverseProjectionMatrix = controlMesh._inverseProjectionMatrix;
        _projectionMatrix = controlMesh._projectionMatrix;
        _selectedControlPointIndex = controlMesh._selectedControlPointIndex;
        m_hasSelectedControlPoint = controlMesh.m_hasSelectedControlPoint;
    }

    META_Object( NURBS, ves::xplorer::scenegraph::nurbs::NURBSControlMesh )

    ///Set the control points
    void SetControlPoints( std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> controlPoints,
                           unsigned int numU,
                           unsigned int numV, bool isSurface = false )
    {
        _controlPoints = &controlPoints;
        _numUControlPoints = numU;
        _numVControlPoints = numV;
        _isSurface = isSurface;
        _selection = false;
    }

    ///Turn on/off selection of control points
    void SetSelection( bool trueFalse );

    ///Update the current mouse position
    ///\param xPosition X mouse position
    ///\param yPosition Y mouse position
    void SetMousePosition( float xPosition, float yPosition );

    ///Do the selection
    ///\param projectionMatrix The current projection matrix
    ///\param modelviewMatrix The current modelview Matrix.
    void Selection()const;

    ///Reset the selected control point index
    void ResetSelection();

    ///Translate the selected control point, if one is selected
    ///\param dx Delta x
    ///\param dy Delta Y
    ///\param dz Delta Z
    bool TranslateSelectedControlPoint( float dx,
                                        float dy,
                                        float dz );

    ///Get control point selection status
    bool IsControlPointSelected();

    // the draw immediate mode method is where the OSG wraps up the drawing of
    // of OpenGL primitives.
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
    virtual void drawImplementation( osg::RenderInfo& currentState ) const;
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
    virtual void drawImplementation( osg::State& currentState ) const;
#endif
    ///Are we in selection mode?
    bool IsSelecting();

    ///Get the indexed control point
    ves::xplorer::scenegraph::nurbs::ControlPoint& GetControlPoint( unsigned int index );

    ///Get the indexed control point
    ves::xplorer::scenegraph::nurbs::ControlPoint& GetSelectedControlPoint();

    // we need to set up the bounding box of the data too, so that the scene graph knows where this
    // objects is, for both positioning the camera at start up, and most importantly for culling.
    ///???
    virtual osg::BoundingBox computeBound() const;

protected:
    ///Destructor
    virtual ~NURBSControlMesh()
    {}

    ///Draw the u iso-curves
    void _drawUCurves()const;

    ///Draw the v iso-curves
    void _drawVCurves()const;

    ///Draw the UV Control Points
    void _drawUVPoints()const;

    bool _selection;///<Flag for selecting ctrl points;
    mutable bool m_hasSelectedControlPoint;///<True if a control point is selected

    bool _isSurface;///<Flag for determining NURBS type.
    mutable double* _projectionMatrix;///<The current projection matrix
    mutable double* _modelViewMatrix;///<The current modelview matrix
    mutable osg::Matrixf _inverseModelViewMatrix;///<The current inverse modelview matrix
    mutable osg::Matrixf _inverseProjectionMatrix;///<The current inverse modelview matrix
    mutable int _selectedControlPointIndex;///<The currently selected control point
    GLfloat _mouse[2];///<The mouse position.
    GLuint _selectionBuffer[512];///<Set Up A Selection Buffer
    GLint _hits;///<Selected primitives
    unsigned int _numUControlPoints;///<The number of control points in U direction.
    unsigned int _numVControlPoints;///<The number of control points in V direction.
    std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint>* _controlPoints;///<The control points

};
}
}
}
}

////////////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::SetSelection( bool trueFalse )
{
    _selection = trueFalse;
}
////////////////////////////////////////////////////////////////////////////////
bool NURBSControlMesh::IsSelecting()
{
    return _selection;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::ControlPoint& NURBSControlMesh::GetSelectedControlPoint()
{
    try
    {
        return _controlPoints->at( _selectedControlPointIndex );
    }
    catch ( ... )
    {
        std::cout << "Invalid control point index." << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::ControlPoint& NURBSControlMesh::GetControlPoint( unsigned int index )
{
    return _controlPoints->at( index );
}
////////////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::_drawUCurves()const
{
    //u iso-curves
    for( unsigned int v = 0; v < _numVControlPoints; v++ )
    {
        glBegin( GL_LINE_STRIP );
        for( unsigned int u = 0; u < _numUControlPoints; u++ )
        {
            //osg::Vec3 nextPoint;
            glVertex3f( _controlPoints->at( v*_numUControlPoints + u ).X(),
                        _controlPoints->at( v*_numUControlPoints + u ).Y(),
                        _controlPoints->at( v*_numUControlPoints + u ).Z() );
        }
        glEnd();
    }
}
////////////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::_drawVCurves()const
{

    //v iso-curves
    for( unsigned int u = 0; u < _numUControlPoints; u++ )
    {
        glBegin( GL_LINE_STRIP );
        for( unsigned int v = 0; v < _numVControlPoints; v++ )
        {
            glVertex3f( _controlPoints->at( v*_numUControlPoints + u ).X(),
                        _controlPoints->at( v*_numUControlPoints + u ).Y(),
                        _controlPoints->at( v*_numUControlPoints + u ).Z() );
        }
        glEnd();

    }
}
////////////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::_drawUVPoints()const
{
    //u iso-curves
    //if(!_selection)
    {
        glEnable( GL_POINT_SMOOTH );

    }
    glPointSize( 5.0 );


    for( unsigned int v = 0; v < _numVControlPoints; v++ )
    {
        for( unsigned int u = 0; u < _numUControlPoints; u++ )
        {
            glLoadName( v*_numUControlPoints + u );
            //_controlPoints->at(v*_numUControlPoints + u).SetRowColumnIndex(v,u);
            glBegin( GL_POINTS );
            glVertex3f( _controlPoints->at( v*_numUControlPoints + u ).X(),
                        _controlPoints->at( v*_numUControlPoints + u ).Y(),
                        _controlPoints->at( v*_numUControlPoints + u ).Z() );
            glEnd();
        }
    }


    //if(!_selection)
    {
        glDisable( GL_POINT_SMOOTH );
    }
}
////////////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::SetMousePosition( float xPosition,
                                         float yPosition )
{
    _mouse[0] = ( xPosition + 1.0 ) * .5;
    _mouse[1] = ( yPosition + 1.0 ) * .5;
}
////////////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::Selection()const
{
    GLint viewport[4];
    GLuint* selectionBuffer = new GLuint[_controlPoints->size()<<2];
    GLint hits;

    glSelectBuffer( _controlPoints->size() << 2, selectionBuffer );

    //get the current viewport
    glGetIntegerv( GL_VIEWPORT, viewport );
    hits = glRenderMode( GL_SELECT );
    //hits = glRenderMode(GL_RENDER);
    glInitNames();
    glPushName( 0 );

    //push the projection matrix
    glMatrixMode( GL_PROJECTION );
    glPushMatrix();

    //clear the projection matrix
    glLoadIdentity();

    gluPickMatrix( viewport[2]*_mouse[0], viewport[3]*_mouse[1],
                   //gluPickMatrix( _mouse[0],viewport[3] - _mouse[1],
                   5.0, 5.0, viewport );
    glMultMatrixd( _projectionMatrix );

    glMatrixMode( GL_MODELVIEW );
    //push the modelview matrix
    glPushMatrix();
    glLoadMatrix( _modelViewMatrix );
    _drawUVPoints();

    //pop the modelview matrix
    glPopMatrix();

    //returning to normal rendering mode
    hits = glRenderMode( GL_RENDER );

    if( hits != 0 )
    {
        m_hasSelectedControlPoint = true;
        _selectedControlPointIndex = selectionBuffer[3];
        int depth = selectionBuffer[1];

        for( int loop = 1; loop < hits; loop++ )
        {
            // If This Object Is Closer To Us Than The One We Have Selected
            if( selectionBuffer[loop*4+1] < GLuint( depth ) )
            {
                _selectedControlPointIndex = selectionBuffer[loop*4+3];
                depth = selectionBuffer[loop*4+1];
            }
        }
    }
    else
    {
        //m_hasSelectedControlPoint = false;
    }
    //restoring the original projection matrix
    glMatrixMode( GL_PROJECTION );
    glPopMatrix();

    glMatrixMode( GL_MODELVIEW );
    glFlush();

    GLenum  glerr = glGetError();
    while( glerr != GL_NO_ERROR )
    {
        std::cout << "glGetError:" << gluErrorString( glerr ) << std::endl;
        glerr = glGetError();
    }
    if( selectionBuffer )
    {
        delete selectionBuffer;
        selectionBuffer = 0;
    }
    hits = 0;
}
////////////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::ResetSelection()
{
    _selectedControlPointIndex = -1;
    m_hasSelectedControlPoint = false;
}
////////////////////////////////////////////////////////////////////////////////
bool NURBSControlMesh::IsControlPointSelected( )
{
    return m_hasSelectedControlPoint;
}
////////////////////////////////////////////////////////////////////////////////
bool NURBSControlMesh::TranslateSelectedControlPoint( float dx,
                                                      float dy,
                                                      float dz )
{
    if( _selection )
    {
        if( _selectedControlPointIndex > -1 )
        {
            osg::Vec3 currentPt( _controlPoints->at( _selectedControlPointIndex ).X(),
                                 _controlPoints->at( _selectedControlPointIndex ).Y(),
                                 _controlPoints->at( _selectedControlPointIndex ).Z() );
            //transform the point into eye space for translation
            currentPt = currentPt * osg::Matrix( _modelViewMatrix );
            currentPt[0] += dx;
            currentPt[1] += dz;
            currentPt[2] += dy;
            //transform the point back into model space
            currentPt = currentPt * _inverseModelViewMatrix;
            _controlPoints->at( _selectedControlPointIndex ).SetX( currentPt[0] );
            _controlPoints->at( _selectedControlPointIndex ).SetY( currentPt[1] );
            _controlPoints->at( _selectedControlPointIndex ).SetZ( currentPt[2] );

            return true;
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
void NURBSControlMesh::drawImplementation( osg::RenderInfo& renderState ) const
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
void NURBSControlMesh::drawImplementation( osg::State& renderState ) const
#endif
{
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
    osg::State& currentState = *( renderState.getState() );
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
    osg::State& currentState = renderState;
#endif
    if( _selection && !m_hasSelectedControlPoint )
    {
        _projectionMatrix = const_cast<double*>( currentState.getProjectionMatrix().ptr() );
        _modelViewMatrix = const_cast<double*>( currentState.getModelViewMatrix().ptr() );
        _inverseModelViewMatrix.invert( currentState.getModelViewMatrix() );
        _inverseProjectionMatrix.invert( currentState.getProjectionMatrix() );
        Selection();
    }
    if( _isSurface )
    {
        //_drawUCurves();
        //_drawVCurves();
    }
    else
    {
        if( _numUControlPoints > _numVControlPoints )
        {
            //_drawUCurves();
        }
        else
        {
            _drawVCurves();
        }
    }
    _drawUVPoints();
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingBox NURBSControlMesh::computeBound()const
{
    osg::BoundingBox bbox;
    if( _controlPoints->size() )
    {
        unsigned int nControlPoints = _controlPoints->size();
        for( unsigned int i = 0; i < nControlPoints; i++ )
        {
            bbox.expandBy( _controlPoints->at( i ).X(), _controlPoints->at( i ).Y(), _controlPoints->at( i ).Z() );
        }
    }
    return bbox;
}
////////////////////////////////////////////////////////////////////////////////

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
///???
class TestControlPointCallback : public osg::NodeCallback
{
public:

    ///???
    TestControlPointCallback( double period ):
            _firstCall( true ),
            _startTime( 0.0 ),
            _time( 0.0 ),
            _period( period )
    {}

    ///???
    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv )
    {

        osg::ref_ptr<NURBSNode> nurbsData = dynamic_cast<NURBSNode*>( node );
        if( nurbsData.valid() && nurbsData->IsSelecting() )
        {
            /*const osg::FrameStamp* fs = nv->getFrameStamp();
            double referenceTime = fs->getReferenceTime();
            if (_firstCall)
            {
               _firstCall = false;
               _startTime = referenceTime;
            }

             _time = referenceTime-_startTime;
             const float TwoPI=2.0f*osg::PI;
             const float phase = -_time/_period;
             ves::xplorer::scenegraph::nurbs::ControlPoint* movingPoint =
                     nurbsData->GetNURBS()->GetControlPoint(3);

             double dz = sinf(TwoPI*phase);
             nurbsData->GetNURBS()->GetControlPoint(3)->Translate(0,0,dz);
             nurbsData->UpdateControlMesh();

             nurbsData->GetNURBS()->UpdateMesh(*(nurbsData->GetNURBS()->GetControlPoint()));*/
        }
    }

    bool    _firstCall;///<???

    double  _startTime;///<???
    double  _time;///<???

    double  _period;///<???
    double  _xphase;///<???
    float   _amplitude;///<???
};
}
}
}
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
////////////////////////////////////////////////////////////////////
///Class for the tessellated surface mesh drawable                //
////////////////////////////////////////////////////////////////////
class VE_NURBS_EXPORTS NURBSTessellatedSurface: public osg::Drawable
{
public:
    ///Constructor
    NURBSTessellatedSurface( ves::xplorer::scenegraph::nurbs::NURBSObject* nurbsObject = 0 )
    {
        _nurbsObject = nurbsObject;
        setUseDisplayList( false );
    }

    ///Copy constructor
    NURBSTessellatedSurface( const NURBSTessellatedSurface& tessSurf,
                             const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY ):
            osg::Drawable( tessSurf, copyop )
    {
        _nurbsObject = tessSurf._nurbsObject;
    }

    META_Object( VE_NURBS, ves::xplorer::scenegraph::nurbs::NURBSTessellatedSurface )

    ///Get the raw NURBS data
    ves::xplorer::scenegraph::nurbs::NURBSObject* GetNURBSData();

    // the draw immediate mode method is where the OSG wraps up the drawing of
    // of OpenGL primitives.
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
    virtual void drawImplementation( osg::RenderInfo& currentState ) const;
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
    virtual void drawImplementation( osg::State& currentState ) const;
#endif

    // we need to set up the bounding box of the data too, so that the scene graph knows where this
    // objects is, for both positioning the camera at start up, and most importantly for culling.
    ///???
    virtual osg::BoundingBox computeBound() const;

protected:
    ///Destructor
    virtual ~NURBSTessellatedSurface()
    {}

    ///Tessellate the surface
    void _tessellateSurface()const;

    ///Tessellate the curve
    void _tessellateCurve() const;

    ///Calculate the normal on the surface at point.
    ///Should this be in the ves::xplorer::scenegraph::nurbs::NURBSSurface class?
    ///\param index ???
    osg::Vec3 _calculateSurfaceNormalAtPoint( unsigned int index )const;

    ves::xplorer::scenegraph::nurbs::NURBSObject* _nurbsObject;///The NURBS representation
};
}
}
}
}

////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::NURBSObject* NURBSTessellatedSurface::GetNURBSData()
{
    return _nurbsObject;
}

////////////////////////////////////////////////////////////////////////////////
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
void NURBSTessellatedSurface::drawImplementation( osg::RenderInfo& renderState ) const
#elif ((OSG_VERSION_MAJOR<=1) && (OSG_VERSION_MINOR<=2))
void NURBSTessellatedSurface::drawImplementation( osg::State& renderState ) const
#endif
{
    if( _nurbsObject->GetType() == NURBSObject::Surface )
    {
        _tessellateSurface();
    }
    else
    {
        _tessellateCurve();
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingBox NURBSTessellatedSurface::computeBound()const
{
    osg::BoundingBox bbox;
    if( _nurbsObject )
    {
        std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> ctPts = _nurbsObject->ControlPoints();
        unsigned int nControlPoints = ctPts.size();
        for( unsigned int i = 0; i < nControlPoints; i++ )
        {
            bbox.expandBy( ctPts[i].X(), ctPts[i].Y(), ctPts[i].Z() );
        }
    }
    return bbox;
}
////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////
///Tessellate the surface                       //
//////////////////////////////////////////////////
void NURBSTessellatedSurface::_tessellateSurface()const
{
    if( _nurbsObject->InterpolatedPoints().empty() )
    {
        std::cout << "Invalid NURBSObject!!" << std::endl;
        std::cout << "NURBSRenderer::_tessellateSurface()" << std::endl;
        return;
    }

    unsigned int nUPoints = _nurbsObject->NumInterpolatedPoints( "U" );
    unsigned int nVPoints = _nurbsObject->NumInterpolatedPoints( "V" );

    //std::cout<<"Num u interpolated points:"<<nUPoints<<std::endl;
    //std::cout<<"Num v interpolated points:"<<nVPoints<<std::endl;
    Point tempPoint;
    for( unsigned int v = 0; v </*2;*/ nVPoints - 1;v++ )
    {
        //new tristrip
        glBegin( GL_TRIANGLE_STRIP );
        //Handle the special case for the first triangle in the strip

        //bottom corner vert
        glNormal3fv( _calculateSurfaceNormalAtPoint( v*nUPoints ).ptr() );
        tempPoint = _nurbsObject->InterpolatedPoints().at( v * nUPoints );
        glVertex3f( tempPoint.X(), tempPoint.Y(), tempPoint.Z() );

        //right corner vert
        glNormal3fv( _calculateSurfaceNormalAtPoint(( v + 1 )*nUPoints ).ptr() );
        tempPoint = _nurbsObject->InterpolatedPoints().at(( v + 1 ) * nUPoints );
        glVertex3f( tempPoint.X(), tempPoint.Y(), tempPoint.Z() );

        //next top vert
        glNormal3fv( _calculateSurfaceNormalAtPoint( v*nUPoints + 1 ).ptr() );
        tempPoint = _nurbsObject->InterpolatedPoints().at(( v ) * nUPoints + 1 );
        glVertex3f( tempPoint.X(), tempPoint.Y(), tempPoint.Z() );

        //interior points
        for( unsigned int u = 1; u < nUPoints - 1; u++ )
        {
            glNormal3fv( _calculateSurfaceNormalAtPoint(( v + 1 )*nUPoints + u ).ptr() );
            tempPoint = _nurbsObject->InterpolatedPoints().at(( v + 1 ) * nUPoints + u );
            glVertex3f( tempPoint.X(),
                        tempPoint.Y(),
                        tempPoint.Z() );

            glNormal3fv( _calculateSurfaceNormalAtPoint(( v )*nUPoints + ( u + 1 ) ).ptr() );
            tempPoint = _nurbsObject->InterpolatedPoints().at( v * nUPoints + ( u + 1 ) );
            glVertex3f( tempPoint.X(),
                        tempPoint.Y(),
                        tempPoint.Z() );
        }
        //handle last point
        glNormal3fv( _calculateSurfaceNormalAtPoint(( v + 1 )*nUPoints + ( nUPoints - 1 ) ).ptr() );
        tempPoint = _nurbsObject->InterpolatedPoints().at(( v + 1 ) * nUPoints + nUPoints - 1 );
        glVertex3f( tempPoint.X(), tempPoint.Y(), tempPoint.Z() );
        glEnd();
    }
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////
///Tessellate the curve                       //
////////////////////////////////////////////////
void NURBSTessellatedSurface::_tessellateCurve()const
{
    if( _nurbsObject->InterpolatedPoints().empty() )
    {
        std::cout << "Invalid NURBSObject!!" << std::endl;
        std::cout << "NURBSTessellatedSurface::_tessellateCurve()" << std::endl;
        return;
    }
    unsigned int nUPoints = _nurbsObject->NumInterpolatedPoints( "U" );
    Point tempPoint;
    //new linestrip
    glBegin( GL_LINE_STRIP );
    for( unsigned int u = 0; u < nUPoints; u++ )
    {
        tempPoint = _nurbsObject->InterpolatedPoints().at( u );
        glVertex3f( tempPoint.X(), tempPoint.Y(), tempPoint.Z() );
    }
    glEnd();
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec3 NURBSTessellatedSurface::_calculateSurfaceNormalAtPoint( unsigned int index )const
{
    osg::Vec3 normal( 0, 1, 0 );
    if( _nurbsObject->GetMinimumDegree() > 1 )
        //if(_nurbsObject->GetType() == NURBSObject::Surface)
    {
        ves::xplorer::scenegraph::nurbs::NURBSSurface* surface = static_cast<ves::xplorer::scenegraph::nurbs::NURBSSurface*>( _nurbsObject );

        ves::xplorer::scenegraph::nurbs::Point dSdV = surface->GetSurfaceDerivatives()[1][0].at( index );
        ves::xplorer::scenegraph::nurbs::Point dSdU = surface->GetSurfaceDerivatives()[0][1].at( index );
        ves::xplorer::scenegraph::nurbs::Point cross = dSdV ^ dSdU;
        normal.set( cross.X(), cross.Y(), cross.Z() );
        normal.normalize();
    }
    return normal;
}
////////////////////////////////////////////////////////////////////////////////
NURBSNode::NURBSNode( ves::xplorer::scenegraph::nurbs::NURBSObject* object )
{
    _nurbsObject = object;
    _isSelecting = false;
    if( _nurbsObject )
    {

        _controlMeshGeode = new osg::Geode();

        _controlMeshDrawable = new ves::xplorer::scenegraph::nurbs::NURBSControlMesh( &_nurbsObject->ControlPoints(),
                               _nurbsObject->NumControlPoints( "U" ),
                               _nurbsObject->NumControlPoints( "V" ),
                               ( _nurbsObject->GetType() == ves::xplorer::scenegraph::nurbs::NURBSObject::Surface ) ? true : false );

        _controlMeshGeode->addDrawable( _controlMeshDrawable.get() );
        _controlMeshGeode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
        osg::ref_ptr<osg::Material> yellow = new osg::Material();
        yellow->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 1, 1, 0, 0 ) );
        _controlMeshGeode->getOrCreateStateSet()->setAttribute( yellow.get() );
        addChild( _controlMeshGeode.get() );

        _triangulatedSurfaceGeode = new osg::Geode();
        _triangulatedSurfaceDrawable = new ves::xplorer::scenegraph::nurbs::NURBSTessellatedSurface( _nurbsObject );
        _triangulatedSurfaceGeode->addDrawable( _triangulatedSurfaceDrawable.get() );

        osg::ref_ptr<osg::ShadeModel> shadeModel = new osg::ShadeModel();
        shadeModel->setMode( osg::ShadeModel::SMOOTH );

        osg::ref_ptr<osg::StateSet> surfaceState = _triangulatedSurfaceGeode->getOrCreateStateSet();
        surfaceState->setAttributeAndModes( shadeModel.get(), osg::StateAttribute::ON );

        addChild( _triangulatedSurfaceGeode.get() );
        //setUpdateCallback(new TestControlPointCallback(1.0));
    }
}
////////////////////////////////////////////////////////////////////////////////
NURBSNode::~NURBSNode()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NURBSNode::NURBSNode( const NURBSNode& input, const osg::CopyOp& copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////
///Show the triangulated wireframe surface //
/////////////////////////////////////////////
void NURBSNode::ViewWireframe( bool trueFalse )
{
    _wireframeView = trueFalse;
}
////////////////////////////////////////////////////////////////////////////////
void NURBSNode::MoveSelectedControlPoint( float dx,
                                          float dy,
                                          float dz )
{
    if( _controlMeshDrawable.valid() )
    {
        if( _controlMeshDrawable->TranslateSelectedControlPoint( dx, dy, dz ) )
        {
            UpdateControlMesh();

            _nurbsObject->UpdateMesh( _controlMeshDrawable->GetSelectedControlPoint() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////
///Get the original surface            //
/////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::NURBSObject* NURBSNode::GetNURBS()
{
    return _nurbsObject;
}
////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////
///Get the osg::Geometry for the surface     //
///////////////////////////////////////////////
osg::Geode* NURBSNode::GetTriangulatedSurface()
{
    if( _triangulatedSurfaceGeode.valid() )
    {
        return _triangulatedSurfaceGeode.get();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////
///Get the control mesh              //
///////////////////////////////////////
osg::Geode* NURBSNode::GetControlMesh()
{
    if( _controlMeshGeode.valid() )
    {
        return _controlMeshGeode.get();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////
///Draw the control mesh          //
////////////////////////////////////
void NURBSNode::UpdateControlMesh()
{
    _triangulatedSurfaceDrawable->dirtyBound();
    //_triangulatedSurfaceDrawable->dirtyDisplayList();
    _controlMeshDrawable->dirtyBound();
    //_controlMeshDrawable->dirtyDisplayList();
}
////////////////////////////////////////////////////////////////////////////////
void NURBSNode::SetSelectionStatus( bool trueFalse )
{
    _isSelecting = trueFalse;

    if( _controlMeshDrawable.valid() )
    {
        if( !_isSelecting )
        {
            _controlMeshDrawable->ResetSelection();
        }
        _controlMeshDrawable->SetSelection( _isSelecting );
    }
}
////////////////////////////////////////////////////////////////////////////////
void NURBSNode::SetMousePosition( float xPosition,
                                  float yPosition )
{
    if( _controlMeshDrawable.valid() )
    {
        _controlMeshDrawable->SetMousePosition( xPosition, yPosition );
    }
}
////////////////////////////////////////////////////////////////////////////////
bool NURBSNode::IsControlPointSelected()
{
    return _controlMeshDrawable->IsControlPointSelected();
}
////////////////////////////////////////////////////////////////////////////////
bool NURBSNode::IsSelecting()
{
    return _isSelecting;
}
////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///Calculate the surface normal at a point                           //
///////////////////////////////////////////////////////////////////////
osg::Vec3 NURBSNode::_calculateSurfaceNormalAtPoint( unsigned int index )
{
    return osg::Vec3( 0., 1., 0. );
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingSphere NURBSNode::computeBound()const
{
    if( _controlMeshDrawable.valid() )
    {
        return osg::BoundingSphere( _controlMeshDrawable->computeBound() );
    }
    return osg::BoundingSphere();
}
////////////////////////////////////////////////////////////////////////////////
