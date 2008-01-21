/*************** <auto-copyright.rb BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> **************/
#include <ves/xplorer/scenegraph/nurbs/NURBSDrawable.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSObject.h>
#include <ves/xplorer/scenegraph/nurbs/NSurface.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <osg/Geometry>
#include <osg/ShadeModel>
#include <osg/Material>

#include <iostream>

using namespace ves::xplorer::scenegraph::nurbs;
//////////////////////////////////////////////////////////////////////////
NURBSDrawable::NURBSDrawable( ves::xplorer::scenegraph::nurbs::NURBSObject*
                                 nurbsObject )
{
    m_nurbsObject = nurbsObject;
    setUseVertexBufferObjects( true );
    _updateTessellatedSurface();
    _initializeStateSet();
}
///////////////////////////////////////////////////////////
NURBSDrawable::NURBSDrawable( const NURBSDrawable& tessSurf,
                              const osg::CopyOp& copyop )
:osg::Geometry( tessSurf, copyop )
{
    m_nurbsObject = tessSurf.m_nurbsObject;
    _updateTessellatedSurface();
}
/////////////////////////////////////////
void NURBSDrawable::_initializeStateSet()
{
    osg::ref_ptr<osg::ShadeModel> shadeModel = new osg::ShadeModel();
    shadeModel->setMode( osg::ShadeModel::SMOOTH );

    osg::ref_ptr<osg::StateSet> surfaceState = getOrCreateStateSet();
    surfaceState->setAttribute( shadeModel.get() );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::NURBSObject* NURBSDrawable::GetNURBSData()
{
    return m_nurbsObject;
}
///////////////////////////////////////////////////////////////////////
void NURBSDrawable::UpdateMesh( NURBSObject* nurbs )
{
    std::vector<unsigned int>::iterator itr;
    size_t nChangedVerts = nurbs->GetChangedTessellatedVertexIndecies().size();
    std::vector<unsigned int> changedVerts = nurbs->GetChangedTessellatedVertexIndecies();
    for( size_t i = 0; i < nChangedVerts; ++i )
    {
        (*m_tessellatedPoints)[changedVerts.at(i)].set(osg::Vec3d( nurbs->InterpolatedPoints().at(changedVerts.at(i)).X(),
                                                                   nurbs->InterpolatedPoints().at(changedVerts.at(i)).Y(),
                                                                   nurbs->InterpolatedPoints().at(changedVerts.at(i)).Z() ) );
        (*m_normals)[changedVerts.at(i)].set( _calculateSurfaceNormalAtPoint( changedVerts.at( i ) ) );
    }
    m_tessellatedPoints->dirty();
    m_normals->dirty();
    dirtyBound();
}
////////////////////////////////////////////////////////
void NURBSDrawable::_updateTessellatedSurface()
{
    //Assuming number of tessellated points doesn't change!!!
    size_t numTessellatedPoints = m_nurbsObject->InterpolatedPoints().size();
    if( !m_tessellatedPoints.valid() )
    {
        //std::cout<<"Num tessellated points: "<<numTessellatedPoints<<std::endl;
        m_tessellatedPoints = new osg::Vec3Array( );
        m_normals = new osg::Vec3Array( );
        m_texCoords = new osg::Vec2Array( );
        setVertexArray( m_tessellatedPoints.get() );
        setNormalArray( m_normals.get() );
        setNormalBinding(osg::Geometry::BIND_PER_VERTEX);
        setTexCoordArray(0, m_texCoords.get() );
    }
    else
    {
       m_tessellatedPoints->clear();
       m_normals->clear();
       m_texCoords->clear();
       removePrimitiveSet(0,getNumPrimitiveSets());
       m_tessellatedPoints->dirty();
       m_texCoords->dirty();
       m_normals->dirty();
    }
    for(size_t i = 0; i < numTessellatedPoints; ++i)
    {
        m_tessellatedPoints->push_back( osg::Vec3d( m_nurbsObject->InterpolatedPoints().at(i).X(),
                                                    m_nurbsObject->InterpolatedPoints().at(i).Y(),
                                                    m_nurbsObject->InterpolatedPoints().at(i).Z() ) );
        m_texCoords->push_back( osg::Vec2d( m_nurbsObject->GetUVParameters().at(i).X(),
                                            m_nurbsObject->GetUVParameters().at(i).Y() ) );
        m_normals->push_back( _calculateSurfaceNormalAtPoint(i) );
        
    } 
    if( m_nurbsObject->GetType() == NURBSObject::Surface )
    {
        _updateSurfacePrimitive();
    }
    else
    {
        _updateCurvePrimitive();
    }
}
/////////////////////////////////////////////////////
void NURBSDrawable::_updateCurvePrimitive()
{
    unsigned int nUPoints = m_nurbsObject->NumInterpolatedPoints( "U" );
    //std::cout<<"curve points: "<<nUPoints<<std::endl;
    //set up the linestrip for the u iso-curves
    osg::DrawElementsUShort& drawElements =
               *( new osg::DrawElementsUShort( GL_LINE_STRIP, nUPoints ) );

    unsigned int index = 0;
    addPrimitiveSet( &drawElements );
    for( unsigned int u = 0; u < nUPoints; ++u )
    {
        drawElements[index++] = u; 
    }
}
///////////////////////////////////////////////////////
void NURBSDrawable::_updateSurfacePrimitive()
{
    unsigned int nUPoints = m_nurbsObject->NumInterpolatedPoints( "U" );
    unsigned int nVPoints = m_nurbsObject->NumInterpolatedPoints( "V" );


    //std::cout<<"Num u interpolated points:"<<nUPoints<<std::endl;
    //std::cout<<"Num v interpolated points:"<<nVPoints<<std::endl;
    for( unsigned int v = 0; v < nVPoints - 1;++v )
    {
        //std::cout<<"v: "<<v<<std::endl;
        osg::DrawElementsUShort& drawElements =
                *( new osg::DrawElementsUShort( GL_TRIANGLE_STRIP,
                                                2*nUPoints ) );
        unsigned int index = 0;
        drawElements[index++] = ( ( v )*nUPoints );
        drawElements[index++] = ( ( v + 1 )*nUPoints );
        drawElements[index++] = ( ( v )*nUPoints + 1 );
        
        //interior points
        for( unsigned int u = 1; u < nUPoints-1 ; u++ )
        {
            drawElements[index++] = ( ( v + 1 )*nUPoints + u );
            drawElements[index++] = ( ( v )*nUPoints + u + 1);
            //drawElements[index++] = ( ( v )*nUPoints + u );
           // std::cout<<( v + 1 )*nUPoints + u<<" "<<( v )*nUPoints + u <<std::endl;
        }
        drawElements[index++] = ( ( v + 1 )*nUPoints + (nUPoints - 1) );
        addPrimitiveSet( &drawElements );
    }

}
/////////////////////////////////////////////////////////////////////////////
osg::Vec3 NURBSDrawable::_calculateSurfaceNormalAtPoint( unsigned int index )
{
    osg::Vec3 normal( 0, 1, 0 );
    if( m_nurbsObject->GetMinimumDegree() > 1 && (m_nurbsObject->GetType() == NURBSObject::Surface ) )
    {
        ves::xplorer::scenegraph::nurbs::NURBSSurface* surface =
               static_cast<ves::xplorer::scenegraph::nurbs::NURBSSurface*>( m_nurbsObject );

        ves::xplorer::scenegraph::nurbs::Point dSdV = surface->GetSurfaceDerivatives()[1][0].at( index );
        ves::xplorer::scenegraph::nurbs::Point dSdU = surface->GetSurfaceDerivatives()[0][1].at( index );
        ves::xplorer::scenegraph::nurbs::Point cross = dSdV ^ dSdU;
        normal.set( cross.X(), cross.Y(), cross.Z() );
        normal.normalize();
        //std::cout<<"Normal: "<<normal.x()<<" "<<normal.y()<<" "<<normal.z()<<std::endl;
    }
    return normal;
}
