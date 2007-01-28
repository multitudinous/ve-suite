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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_DCS_H
#define VE_DCS_H
/*!\file DCS.h
* DCS API
*/

/*!\class VE_SceneGraph::DCS
*
*/
#include "VE_Xplorer/SceneGraph/SceneNode.h"

#include <gmtl/Matrix.h>
#include <gmtl/Quat.h>

#ifdef _PERFORMER
class pfDCS;
#elif _OSG
#include <osg/PositionAttitudeTransform>
#elif _OPENSG
#endif

//C/C++ Libraries
#include <vector>
#include <string>

class btTransform;

namespace VE_SceneGraph
{
#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS DCS: public osg::PositionAttitudeTransform, public SceneNode
#elif _PERFORMER
class VE_SCENEGRAPH_EXPORTS DCS: public pfDCS, SceneNode
#endif
{
public:
   DCS();
   virtual ~DCS();
   DCS( float* scale, float* trans, float* rot );
   DCS( const DCS& input );
   DCS& operator=( const DCS& input );

   float* GetVETranslationArray( void );
   float* GetRotationArray( void );
   float* GetScaleArray( void );
   gmtl::Matrix44f GetMat( void );
   int GetNumChildren( void );
   const std::string GetName( void );

   void SetTranslationArray( std::vector<double> array );
   void SetTranslationArray( float* trans );
   void SetRotationArray( std::vector<double> array);
   void SetRotationArray( float* rot );
   void SetScaleArray( std::vector<double> array );
   void SetScaleArray( float* scale );
   void SetMat( gmtl::Matrix44f& input );
   void SetRotationMatrix( gmtl::Matrix44f& input );
   void SetName( std::string name );

   int RemoveChild( SceneNode* child );
   int AddChild( SceneNode* child );
   void InsertChild( int position, SceneNode* child );
   int ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild );

   ///Get the Bullet transform for this node
   btTransform* GetPhyiscsTransform( void );
   
protected:
   float translation[3];
   float scale[3];
   float rotation[3];
   btTransform* bulletTransform;
private:
   ///Update the bullet matrix with the matrix from the osg node
   /// the osg node gets set first and is then updated by the 
   /// bullet physics simulator
   void UpdatePhysicsTransform( void );   
};
}

#endif //VE_DCS_H
