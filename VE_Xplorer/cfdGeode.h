/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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

#include "cfdSceneNode.h"

#ifdef _PERFORMER
class pfGeode;
#elif _OSG
#elif _OPENSG
#endif
class vtkActor;

//! Iris Performer
/*!
  Update a modified Performer geometry node.
*/
class cfdGeode: public cfdSceneNode
{
   public:
      cfdGeode( void );
      ~cfdGeode( void );
      cfdGeode( const cfdGeode& );
      cfdGeode& operator=( const cfdGeode& );

      // This function will have to reimplmented for each scenegraph
      // Get Geode 
#ifdef _PERFORMER
      pfGeode* GetGeode( void );
      pfNode*  GetRawNode( void );
#elif _OSG
#elif _OPENSG
#endif
      // This function implements the respective translate vtkActorToGeode
      void TranslateTocfdGeode( vtkActor* );

   private:
#ifdef _PERFORMER
      pfGeode* _geode;
#elif _OSG
#elif _OPENSG
#endif
      int _vtkToPFDebug;
};
#endif
