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
 * Id:            $Id: NURBS.cxx 10265 2007-12-26 20:42:01Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSControlMesh.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSDrawable.h>
#include <ves/xplorer/scenegraph/nurbs/NSurface.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <osg/Drawable>
#include <osg/Geometry>
#include <osg/BoundingBox>
#include <osg/BoundingSphere>
#include <osg/ShadeModel>
#include <osg/Material>

#include <iostream>

using namespace ves::xplorer::scenegraph::nurbs;

////////////////////////////////////////////////////////////////////
NURBS::NURBS( ves::xplorer::scenegraph::nurbs::NURBSObject* object )
{
    m_nurbsObject = object;
    if( m_nurbsObject )
    {

        bool isSurface = false;
        if ( m_nurbsObject->GetType() == ves::xplorer::scenegraph::nurbs::NURBSObject::Surface )
        {
            isSurface = true ; 
        }
        m_controlMeshDrawable =
             new ves::xplorer::scenegraph::nurbs::NURBSControlMesh( m_nurbsObject->ControlPoints(),
                                                                    m_nurbsObject->NumControlPoints( "U" ),
                                                                    m_nurbsObject->NumControlPoints( "V" ),
                                                                    isSurface );

        addDrawable( m_controlMeshDrawable.get() );
        osg::ref_ptr<osg::StateSet> ss = m_controlMeshDrawable->getOrCreateStateSet();
        ss->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

        osg::ref_ptr<osg::Material> yellow = new osg::Material();
        yellow->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 1, 1, 0, 0 ) );
        ss->setAttribute( yellow.get() );

        m_nurbsDrawable =
           new ves::xplorer::scenegraph::nurbs::NURBSDrawable( m_nurbsObject );
        addDrawable( m_nurbsDrawable.get() );

        osg::ref_ptr<osg::ShadeModel> shadeModel = new osg::ShadeModel();
        shadeModel->setMode( osg::ShadeModel::SMOOTH );

        osg::ref_ptr<osg::StateSet> surfaceState = 
           m_nurbsDrawable->getOrCreateStateSet();
        surfaceState->setAttributeAndModes( shadeModel.get(), osg::StateAttribute::ON );
    }
}
////////////////////////////////////////////////////////////////////////////////
NURBS::~NURBS()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NURBS::NURBS( const NURBS& input, const osg::CopyOp& copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////
///Show the triangulated wireframe surface //
/////////////////////////////////////////////
void NURBS::ViewWireframe( bool trueFalse )
{
    m_wireframeView = trueFalse;
}
/////////////////////////////////////////
///Get the original surface            //
/////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::NURBSObject* NURBS::GetNURBS()
{
    return m_nurbsObject;
}
