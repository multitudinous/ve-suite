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
   class TransferPhysicsDataCallback;
}

namespace VE_SceneGraph
{
#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS DCS : public osg::PositionAttitudeTransform, public SceneNode
#elif _PERFORMER
class VE_SCENEGRAPH_EXPORTS DCS : public pfDCS, SceneNode
#endif
{
public:
   ///Copy constructor
   DCS();
   ///Destructor
   virtual ~DCS();
   ///Constructor to set user defined transform values
   DCS( float* scale, float* trans, float* rot );
   ///Copy constructor
   DCS( const DCS& input );
   ///Equal operator
   DCS& operator=( const DCS& input );

   ///Get translation array pointer
   float* GetVETranslationArray( void );
   ///Get rotation array pointer
   float* GetRotationArray( void );
   ///Get scale array pointer
   float* GetScaleArray( void );
   ///Get the transform in 4x4 form
   gmtl::Matrix44f GetMat( void );
   ///Generic get number of children
   int GetNumChildren( void );
   ///Set the name of the node
   const std::string GetName( void );

   ///Set the translation array with a vector
   void SetTranslationArray( std::vector<double> array );
   ///Set the translation array
   void SetTranslationArray( float* trans );
   ///set the rotation array with a vector
   void SetRotationArray( std::vector<double> array);
   ///Set the rotation array with a pointer
   void SetRotationArray( float* rot );
   ///set the scale array with a vector
   void SetScaleArray( std::vector<double> array );
   ///set the scale array with a pointer
   void SetScaleArray( float* scale );
   ///Set the rotation matrix for this transfor
   ///this function set all of the scale, translation, and rotation
   ///arrays with the incoming matrix
   void SetMat( gmtl::Matrix44f& input );
   ///Set jsut the roation portion of this transform
   ///with the 4x4 matrix
   ///\param input Matrix with pure rotations
   void SetRotationMatrix( gmtl::Matrix44f& input );
   ///Generic set name function
   void SetName( std::string name );
   ///Generic remove child function
   int RemoveChild( SceneNode* child );
   ///Generic add child function
   int AddChild( SceneNode* child );
   ///Generic insert child function
   void InsertChild( int position, SceneNode* child );
   ///Generic replace child function
   int ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild );
   ///Get the Bullet transform for this node
   btTransform* GetPhysicsTransform( void );
   
protected:
   float translation[3];///<The translation array
   float scale[3];///<The scale array
   float rotation[3];///<The rotation array
   btTransform* bulletTransform;///<The physics transform
private:
   ///Update the bullet matrix with the matrix from the osg node
   /// the osg node gets set first and is then updated by the 
   /// bullet physics simulator
   void UpdatePhysicsTransform( void );
   
   TransferPhysicsDataCallback* udcb;///<The callback to update the sg node with physics data
};
//This is the callback class configured to handle transfering physics data
//back to the respective osg node
#ifdef _OSG
class TransferPhysicsDataCallback : public osg::NodeCallback
{
public:
   ///Constructor
   TransferPhysicsDataCallback();
   ///Destructor
   virtual ~TransferPhysicsDataCallback(){;}
   ///Copy constructor
   TransferPhysicsDataCallback( const TransferPhysicsDataCallback& );
   ///Set the bullet transform for this callback
   ///\param transform bullet transform
   void SetbtTransform( btTransform* transform );
   ///operator required by osg
   virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);
protected:
   btTransform* physicsTransform;///<Pointer to thebullet transform
};
#elif _OPENSG
#endif
}
#endif //VE_DCS_H
