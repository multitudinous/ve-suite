/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_GROUP_H
#define CFD_GROUP_H

#include "VE_SceneGraph/cfdNode.h"
#include <vector>
#ifdef _PERFORMER
class pfGroup;
#elif _OSG
namespace osg { class Group; }
#elif _OPENSG
#endif

class VEPLUGIN_DECLSPEC cfdGroup: public cfdNode
{
   public:
      cfdGroup();
      cfdGroup( const cfdGroup& );

      //cfdGroup(float*, float*, float*);
      virtual ~cfdGroup( void );

      //equal operator
      cfdGroup& operator= ( const cfdGroup& );

      //equality operator
      //bool operator== ( cfdNode& );

      //the wrappers for child/scene graph
      //manipulation
      virtual int RemoveChild( cfdNode* );
      virtual int AddChild( cfdNode* );

      virtual void InsertChild( int, cfdNode* );
      virtual int  SearchChild( cfdNode* );

      virtual cfdNode* GetChild( int );
      virtual int  GetNumChildren( void );

      virtual void SetName( char* );
      virtual int ReplaceChild( cfdNode*, cfdNode* );

      virtual cfdNode* Clone( int );

#ifdef _PERFORMER
      virtual pfNode* GetRawNode( void );
#elif _OSG
      virtual osg::Node* GetRawNode( void );
#elif _OPENSG
#endif

      virtual const char* GetName( void );
   protected:
#ifdef _PERFORMER
      pfGroup* _group;
#elif _OSG
      osg::ref_ptr<osg::Group> _group;
#elif _OPENSG
#endif
      std::vector< cfdNode* > childNodes;
};
#endif
