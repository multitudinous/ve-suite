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
 * File:          $RCSfile: cfdGroup.h,v $
 * Date modified: $Date: 2004-05-18 13:44:18 -0700 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_GROUP_H
#define CFD_GROUP_H

#include "cfdSceneNode.h"
#include <vector>
 
class pfGroup;

class cfdGroup: public cfdSceneNode
{
   public:
      cfdGroup( void );
      cfdGroup( float*, float*, float* );
      cfdGroup( const cfdGroup& );
      cfdGroup& operator=( const cfdGroup& );
      ~cfdGroup( void );

      //cfdGroup* GetNode( void );
#ifdef _PERFORMER
		pfNode* GetRawNode( void );
#elif _OSG
#elif _OPENSG
#endif
      int RemoveChild( cfdSceneNode* );
      int AddChild( cfdSceneNode* );

      void InsertChild( int, cfdSceneNode* );
      int  SearchChild( cfdSceneNode* );
      cfdSceneNode* GetChild( int );
      int  GetNumChildren( void );
      void SetName( char* );
      int ReplaceChild( cfdSceneNode*, cfdSceneNode* );
      cfdSceneNode* Clone( int );

   private:
      float _translation[ 3 ];
      float _rotation[ 3 ];
      float _scale[ 3 ];
   
      std::vector< cfdSceneNode* > childNodes;
#ifdef _PERFORMER
      pfGroup* _group;
#elif _OSG
#elif _OPENSG
#endif
};
#endif
