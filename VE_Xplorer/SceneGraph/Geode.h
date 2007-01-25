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
 * Date modified: $Date: 2006-07-28 20:47:10 -0500 (Fri, 28 Jul 2006) $
 * Version:       $Rev: 5067 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_GEODE_H
#define VE_GEODE_H
/*!\file Geode.h
* Geode API
*/

/*!\class VE_SceneGraph::Geode
*
*/
#include "VE_Xplorer/SceneGraph/SceneNode.h"

#ifdef _PERFORMER
//#include <pf/>
#elif _OSG
#include <osg/Geode>
#include <osg/ref_ptr>
#elif _OPENSG
#endif

class vtkActor;

namespace VE_SceneGraph
{
#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS Geode: public osg::Geode, public SceneNode
#elif _PERFORMER
class VE_SCENEGRAPH_EXPORTS Geode: public pfDCS
#endif
{
public:
   Geode( void );
   ~Geode( void );
   Geode( const Geode& );        
   Geode& operator=( const Geode& );
   
   #ifdef _PERFORMER
   Geode( const pfGeode& geode );
   Geode& operator=( const pfGeode& geode );
   pfNode* GetRawNode( void );
   #elif _OSG
   Geode( const osg::Geode& geode );
   Geode& operator=( const osg::Geode& geode );
   osg::Node* GetRawNode( void );
   #elif _OPENSG
   #endif

   void TurnOnDebugOutput( int onOff = 0 ){ _vtkDebugLevel = onOff; }

   //This function implements the respective translate vtkActorToGeode
   void TranslateToGeode( vtkActor* actor );

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

#endif //VE_GEODE_H
