/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/scenegraph/Geode.h>

#include <ves/xplorer/cfdDebug.h>

#ifdef _PERFORMER
#include <Performer/pf/pfGeode.h>
#include <Performer/pf/pfNode.h>
#include <ves/xplorer/scenegraph/vtkActorToPF.h>
#elif _OSG
#include <ves/xplorer/scenegraph/vtkActorToOSG.h>

#include <osg/Geode>
#include <osg/Node>
#include <osg/LightModel>
#include <osg/CopyOp>
#elif _OPENSG
#endif

#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkMapper.h>
#include <vtkPolyDataMapper.h>
#include <vtkDataSet.h>

//C/C++ Libraries
#include <iostream>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
Geode::Geode( void )
{
   _vtkDebugLevel = 0;
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _OSG
Geode::Geode(const Geode& geode,const osg::CopyOp& copyop):
osg::Geode(geode,copyop)
{
   ;
}
#endif
////////////////////////////////////////////////////////////////////////////////
Geode::~Geode( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Geode::TranslateToGeode( vtkActor* actor )
{
#ifdef _PERFORMER
   VE_SceneGraph::vtkActorToPF( actor, this, _vtkDebugLevel );
#elif _OSG
   VE_SceneGraph::vtkActorToOSG(actor, this, _vtkDebugLevel);
   osg::ref_ptr< osg::LightModel > lightModel = new osg::LightModel();
   lightModel->setTwoSided( true );
   this->getOrCreateStateSet()->setAttributeAndModes( lightModel.get(), osg::StateAttribute::ON);
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* Geode::GetParent( unsigned int position )
{
#ifdef _OPENSG
   
#elif _OSG
	return this->getParent( position );
#endif
}
////////////////////////////////////////////////////////////////////////////////
