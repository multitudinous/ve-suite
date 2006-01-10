/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: cfdGeode.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_GEODE_H
#define CFD_GEODE_H

#include "VE_SceneGraph/cfdNode.h"

#ifdef _PERFORMER
class pfGeode;
#elif _OSG
namespace osg { class Geode; }
#elif _OPENSG
#endif
class vtkActor;

//! Iris Performer
/*!
  Update a modified Performer geometry node.
*/
namespace VE_SceneGraph{
   class VE_SCENEGRAPH_EXPORTS cfdGeode: public cfdNode
   {
      public:
         cfdGeode( void );
         ~cfdGeode( void );
         cfdGeode( const cfdGeode& );
         cfdGeode& operator=( const cfdGeode& );
   
#ifdef _PERFORMER
         cfdGeode(const pfGeode& geode);
         cfdGeode& operator=(const pfGeode& geode);
         pfNode* GetRawNode( void );
#elif _OSG
         cfdGeode(const osg::Geode& geode);
         cfdGeode& operator=(const osg::Geode& geode);
         osg::Node* GetRawNode( void );
#elif _OPENSG
#endif
         void TurnOnDebugOutput(int onOff = 0){_vtkDebugLevel = onOff;}
         // This function implements the respective translate vtkActorToGeode
         void TranslateTocfdGeode( vtkActor* );
   protected:
         int _vtkDebugLevel;
#ifdef _PERFORMER
         pfGeode* _geode;
#elif _OSG
         osg::ref_ptr<osg::Geode> _geode;
#elif _OPENSG
#endif
    };
}
#endif
