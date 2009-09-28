/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/Geode.h>
#include <ves/xplorer/scenegraph/vtkActorToOSG.h>
#include <ves/xplorer/scenegraph/vtkActorToStreamLine.h>

#include <ves/xplorer/scenegraph/technique/Technique.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Node>
#include <osg/LightModel>
#include <osg/CopyOp>
#include <osgUtil/Optimizer>

// --- VTK Includes --- //
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkMapper.h>
#include <vtkPolyDataMapper.h>
#include <vtkDataSet.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Geode::Geode()
{
    _vtkDebugLevel = 0;
}
////////////////////////////////////////////////////////////////////////////////
Geode::Geode( const Geode& geode, const osg::CopyOp& copyop )
    :
    osg::Geode( geode, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Geode::~Geode()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Geode::TranslateToGeode( vtkActor* actor )
{
    ves::xplorer::scenegraph::vtkActorToOSG( actor, this, _vtkDebugLevel );
    osg::ref_ptr< osg::LightModel > lightModel = new osg::LightModel();
    lightModel->setTwoSided( true );
    getOrCreateStateSet()->setAttributeAndModes( 
        lightModel.get(), osg::StateAttribute::ON );
    osgUtil::Optimizer geodeOpti;
    geodeOpti.optimize( this, 
                       osgUtil::Optimizer::FLATTEN_STATIC_TRANSFORMS |
                       osgUtil::Optimizer::REMOVE_REDUNDANT_NODES |
                       osgUtil::Optimizer::REMOVE_LOADED_PROXY_NODES |
                       osgUtil::Optimizer::COMBINE_ADJACENT_LODS |
                       osgUtil::Optimizer::SHARE_DUPLICATE_STATE |
                       osgUtil::Optimizer::MERGE_GEOMETRY |
                       osgUtil::Optimizer::CHECK_GEOMETRY |
                       osgUtil::Optimizer::SPATIALIZE_GROUPS |
                       osgUtil::Optimizer::TRISTRIP_GEOMETRY |
                       osgUtil::Optimizer::OPTIMIZE_TEXTURE_SETTINGS |
                       osgUtil::Optimizer::MERGE_GEODES |
                       osgUtil::Optimizer::STATIC_OBJECT_DETECTION );
}
////////////////////////////////////////////////////////////////////////////////
void Geode::StreamLineToGeode( vtkActor* actor )
{
    ves::xplorer::scenegraph::vtkActorToStreamLine(
        actor, this, _vtkDebugLevel );
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* Geode::GetParent( unsigned int position )
{
    if( getNumParents() > 0 )
    {
        return getParent( position );
    }
    else
    {
        return 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Geode::traverse( osg::NodeVisitor& nv )
{
    technique::Technique* technique = mTechniques[ mActiveTechnique ];

    technique->Traverse( nv, this );
}
////////////////////////////////////////////////////////////////////////////////
void Geode::InheritedTraverse( osg::NodeVisitor& nv )
{
    typedef osg::Geode inherited;
    inherited::traverse( nv );
}
////////////////////////////////////////////////////////////////////////////////
