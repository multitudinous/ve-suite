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
 * Date modified: $Date: 2007-12-26 14:42:01 -0600 (Wed, 26 Dec 2007) $
 * Version:       $Rev: 10265 $
 * Author:        $Author: biv $
 * Id:            $Id: NURBSNode.cxx 10265 2007-12-26 20:42:01Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/

#include <ves/xplorer/scenegraph/nurbs/NURBSControlMesh.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>

using namespace ves::xplorer::scenegraph::nurbs;
////////////////////////////////////
NURBSControlMesh::NURBSControlMesh()
{
    setUseVertexBufferObjects( true );
    m_numUControlPoints = 1;
    m_numVControlPoints = 0;
    m_isSurface = false;
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
NURBSControlMesh::NURBSControlMesh( std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> controlPoints,
                                    unsigned int numU,
                                    unsigned int numV,
                                    bool isSurface )
{
    m_controlPoints = controlPoints;
    m_numUControlPoints = numU;
    m_numVControlPoints = numV;
    m_isSurface = isSurface;
    setUseVertexBufferObjects( true );
    _updateControlMeshPrimitives();
}
///////////////////////////////////////////////////////////////////////
NURBSControlMesh::NURBSControlMesh( const NURBSControlMesh& controlMesh,
                                    const osg::CopyOp& copyop )
:osg::Geometry( controlMesh, copyop )
{ 
    m_controlPoints = controlMesh.m_controlPoints;
    m_numUControlPoints = controlMesh.m_numUControlPoints;
    m_numVControlPoints = controlMesh.m_numVControlPoints;
    m_isSurface = controlMesh.m_isSurface;
    _updateControlMeshPrimitives();
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::SetControlPoints( std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> controlPoints,
                                         unsigned int numU,
                                         unsigned int numV,
                                         bool isSurface )
{
    m_controlPoints = controlPoints;
    m_numUControlPoints = numU;
    m_numVControlPoints = numV;
    m_isSurface = isSurface;
    _updateControlMeshPrimitives();
}
/////////////////////////////////////////////////////
void NURBSControlMesh::_updateControlMeshPrimitives()
{
    //Assuming number of control points doesn't change!!!
    size_t numControlPoints = m_controlPoints.size();
    if( !m_controlMeshVerts.valid() )
    {
        m_controlMeshVerts = new osg::Vec3Array( );
        setVertexArray(m_controlMeshVerts.get());
    }
    else
    {
       m_controlMeshVerts->clear();
       removePrimitiveSet(0,getNumPrimitiveSets());
       m_controlMeshVerts->dirty();
    }
    for(size_t i = 0; i < numControlPoints; ++i)
    {
        m_controlMeshVerts->push_back( osg::Vec3d( m_controlPoints.at(i).X(),
                                                   m_controlPoints.at(i).Y(),
                                                   m_controlPoints.at(i).Z()));
    }

    if( m_isSurface )
    {
        _updateControlUCurvePrimitives();
        _updateControlVCurvePrimitives();
    }
    else
    {
        if( m_numUControlPoints > m_numVControlPoints )
        {
            _updateControlUCurvePrimitives();
        }
        else
        {
            _updateControlVCurvePrimitives();
        }
    }
    _updateControlPointsPrimitives();
}
//////////////////////////////////////////////////////
void NURBSControlMesh::_updateControlPointsPrimitives()
{
    addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::POINTS,
                                        0, m_controlMeshVerts->size() ) );
}
///////////////////////////////////////////////////////
void NURBSControlMesh::_updateControlUCurvePrimitives()
{
    //set up the linestrip for the u iso-curves
    for( unsigned int v = 0; v < m_numVControlPoints; ++v )
    {
        osg::DrawElementsUShort& drawElements =
              *(new osg::DrawElementsUShort( GL_LINE_STRIP,
                                             m_numUControlPoints ) );
        unsigned int index = 0;
        addPrimitiveSet( &drawElements );
        for( unsigned int u = 0; u < m_numUControlPoints; ++u )
        {
            drawElements[index++] = v*m_numUControlPoints + u;
        }
    }
}
///////////////////////////////////////////////////////
void NURBSControlMesh::_updateControlVCurvePrimitives()
{
    for( unsigned int u = 0; u < m_numUControlPoints; u++ )
    {
        osg::DrawElementsUShort& drawElements = 
                     *( new osg::DrawElementsUShort( GL_LINE_STRIP,
                                                     m_numVControlPoints ) );
        unsigned int index = 0;
        addPrimitiveSet( &drawElements );
        for( unsigned int v = 0; v < m_numVControlPoints; v++ )
        {
            drawElements[index++] = v*m_numUControlPoints + u;
        }
    }
}



