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
 * Id:            $Id: NURBS.cxx 10265 2007-12-26 20:42:01Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> **************/
#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSControlMesh.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSDrawable.h>
#include <ves/xplorer/scenegraph/nurbs/NSurface.h>
#include <ves/xplorer/scenegraph/nurbs/NCurve.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/ControlMeshPointFunctor.h>
#include <osg/Vec3>
#include <osg/Node>
#include <osg/Drawable>
#include <osg/Geometry>
#include <osg/BoundingBox>
#include <osg/BoundingSphere>
#include <osg/ShadeModel>
#include <osg/Material>
#include <osgManipulator/Selection>
#include <osgManipulator/Projector>
#include <osgManipulator/Dragger>

#include <iostream>

using namespace ves::xplorer::scenegraph::nurbs;

////////////////////////////////////////////////////////////////////
NURBS::NURBS( ves::xplorer::scenegraph::nurbs::NURBSObject* object )
:m_isSurface(false)
{
    {
        m_nurbsObject =  object;
        if ( object->GetType() == ves::xplorer::scenegraph::nurbs::NURBSObject::Surface )
        {
            m_isSurface = true ; 
        }
        m_controlMeshDrawable =
             new ves::xplorer::scenegraph::nurbs::NURBSControlMesh( m_nurbsObject->ControlPoints(),
                                                                    m_nurbsObject->NumControlPoints( "U" ),
                                                                    m_nurbsObject->NumControlPoints( "V" ),
                                                                    m_isSurface );
        addDrawable( m_controlMeshDrawable.get() );

        m_nurbsDrawable =
           new ves::xplorer::scenegraph::nurbs::NURBSDrawable( m_nurbsObject );
        addDrawable( m_nurbsDrawable.get() );
    }
    m_selectedControlPointIndex = -1;
}
////////////////////////////////////////////////////////////////////////////////
NURBS::~NURBS()
{
    if( m_nurbsObject )
    {
        delete m_nurbsObject;
        m_nurbsObject = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
NURBS::NURBS( const NURBS& input, const osg::CopyOp& copyop )
:osg::Geode( input, copyop)
{
    m_selectedControlPointIndex = input.m_selectedControlPointIndex;
}
/////////////////////////////////////////
NURBS& NURBS::operator=(const NURBS& rhs)
{
    if( this != &rhs )
    {
        m_selectedControlPointIndex =
             rhs.m_selectedControlPointIndex;
    }
    return *this;
}
/////////////////////////////////////////////
///Show the triangulated wireframe surface //
/////////////////////////////////////////////
void NURBS::ViewWireframe( bool trueFalse )
{
    m_wireframeView = trueFalse;
}
///////////////////////////////////////////////////////////////
///Get the original surface                                  //
///////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::NURBSObject* NURBS::GetNURBS()
{
    return m_nurbsObject;
}
/////////////////////////////////////////////////////////
void NURBS::SetSelectedControlPoint( unsigned int index)
{
    //std::cout<<"Selecting control point: "<<index<<std::endl;
    m_selectedControlPointIndex = index;
    m_nurbsObject->SetMovingControlPoint( m_selectedControlPointIndex );
}
//////////////////////////////////////////
void NURBS::ReleaseControlPointSelection()
{
    m_selectedControlPointIndex = -1;
}
///////////////////////////////////////////////////////////////
void NURBS::MoveSelectedControlPoint( osg::Matrix currentView,
                                      osg::Vec3 relativeMotion )
{
    if( !HasSelectedControlPoint() )
    {
        std::cout<<"No control point selected"<<std::endl;
        return;
    }
    //This method is brute force update of all objects!!!!
    //Eventually need to change this so that updates only happen in
    //NURBSObject and all drawable VBO's get updated appropriately

    osg::Vec3 currentPt( m_nurbsObject->GetControlPoint( m_selectedControlPointIndex )->X(),
                         m_nurbsObject->GetControlPoint( m_selectedControlPointIndex )->Y(),
                         m_nurbsObject->GetControlPoint( m_selectedControlPointIndex )->Z() );

    //transform the point into eye space for translation
    currentPt = currentPt*currentView;
    currentPt += relativeMotion;

    //transform the point back into model space
    osg::Matrix inverse;
    inverse.invert(currentView);
    currentPt = currentPt*( inverse );

    //update the NURBSObject information
    m_nurbsObject->UpdateControlPointPosition( m_selectedControlPointIndex,
                                               Point( currentPt[0],
                                                      currentPt[1],
                                                      currentPt[2] ) );

    m_nurbsObject->UpdateMesh( );

    //update the NURBSControlMesh
    m_controlMeshDrawable->UpdateControlPointPosition( m_selectedControlPointIndex,
                                                       currentPt );
    //updating NURBSDrawable
    m_nurbsDrawable->UpdateMesh( m_nurbsObject );
}
/////////////////////////////////////
bool NURBS::HasSelectedControlPoint()
{
    return ( m_selectedControlPointIndex >= 0 );
}


