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
#ifndef DCS_H
#define DCS_H

/*!\file DCS.h
*/

/*!\class VE_SceneGraph::DCS
*
*/

/*!\namespace VE_SceneGraph
*
*/

// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/SceneNode.h"

namespace VE_SceneGraph
{
   class TransferPhysicsDataCallback;
}

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Matrix>
#include <osg/PositionAttitudeTransform>

namespace osg
{
   class NodeVisitor;
}
#elif _OPENSG
#endif

// --- VR Juggler Includes --- //
#include <gmtl/Matrix.h>
#include <gmtl/Quat.h>

// --- Bullet Includes --- //
class btTransform;
class btRigidBody;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace VE_SceneGraph
{
#ifdef _OSG
class VE_SCENEGRAPH_EXPORTS DCS : public osg::PositionAttitudeTransform, public SceneNode
#elif _OPENSG
#endif
{
public:
   ///Constructor
   DCS( void );

protected:
   ///Destructor
   virtual ~DCS( void );

public:
   ///Constructor to set user defined transform values
   ///\param scale
   ///\param trans
   ///\param rot
   DCS( float* scale, float* trans, float* rot );

   ///Copy constructor using CopyOp to manage deep vs shallow copy
   DCS( const DCS&, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

   META_Node( VE_SceneGraph, DCS );

   ///Get translation array pointer
   float* GetVETranslationArray( void );

   ///Get rotation array pointer
   float* GetRotationArray( void );

   ///Get scale array pointer
   float* GetScaleArray( void );

   ///Get the transform in 4x4 matrix form
   gmtl::Matrix44f GetMat( void );

   ///Set the translation array with a vector
   ///\param transArray
   void SetTranslationArray( std::vector< double > transArray );

   ///Set the translation array
   ///\param trans The translation array pointer
   void SetTranslationArray( float* trans );

   ///Set the attitude with a quat
   ///\param quat
   void SetQuat( osg::Quat quat );

   ///Set the rotation array with a vector
   ///\param rotArray
   void SetRotationArray( std::vector< double > rotArray );

   ///Set the rotation array with a pointer
   ///\param rot
   void SetRotationArray( float* rot );

   ///Set the scale array with a vector
   ///\param scaleArray
   void SetScaleArray( std::vector< double > scaleArray );

   ///Set the scale array with a pointer
   ///\param scale
   void SetScaleArray( float* scale );

   ///Set the matrix for this transform
   ///\param input Matrix with all transform information
   void SetMat( gmtl::Matrix44f& input );

   ///Set just the roation portion of this transform with the 4x4 matrix
   ///\param input Matrix with pure rotations
   void SetRotationMatrix( gmtl::Matrix44f& input );

   ///Generic set name function
   ///\param name
   void SetName( std::string name );

   ///Generic remove child function
   ///\param child
   int RemoveChild( SceneNode* child );

   ///Generic add child function
   ///\param child
   int AddChild( SceneNode* child );

   ///Generic insert child function
   ///\param position
   ///\param child
   void InsertChild( int position, SceneNode* child );

   ///Generic replace child function
   ///\param childToBeReplaced
   ///\param newChild
   int ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild );

   ///Generic search child function
   ///\param searchChild SceneNode* of child to be found
   bool SearchChild( VE_SceneGraph::SceneNode* searchChild );

   ///Generic find parent function
   ///\param position The position of the parent to be returned
   osg::Group* GetParent( unsigned int position );

   ///Generic get child function
   ///\param position The position of the child to be returned
   osg::Node* GetChild( unsigned int position );

   ///Generic get number of children
   int GetNumChildren( void );

   ///Set the name of the node
   const std::string GetName( void );

   ///Toggle the display of this node
   ///\param onOff Toggle the display of this node
   void ToggleDisplay( std::string onOff );

   ///Toggle the display of this node
   ///\param onOff Toggle the display of this node
   void ToggleDisplay( bool onOff );

   ///Set the Bullet rigid body for this node, typically is set from CADEntity
   ///\param rigidBody
   void SetbtRigidBody( btRigidBody* rigidBody );
   
protected:
   float m_Rotation[3];///<The rotation array
   float m_Translation[3];///<The translation array
   float m_Scale[3];///<The scale array

   btRigidBody* m_btBody;///<The rigid body to access the respective btTransform

private:
   ///Update the bullet matrix with the matrix from the osg node
   ///The osg node gets set first and is then updated by the bullet physics simulator
   void UpdatePhysicsTransform( void );
   
   osg::ref_ptr< TransferPhysicsDataCallback > m_udcb;///<The callback to update the sg node with physics data

};

//This is the callback class configured to handle transfering physics data back to the respective osg node
#ifdef _OSG
class TransferPhysicsDataCallback : public osg::NodeCallback
{
public:
   ///Constructor
   TransferPhysicsDataCallback( void );

   ///Destructor
   virtual ~TransferPhysicsDataCallback( void ){;}

   ///Copy constructor
   TransferPhysicsDataCallback( const TransferPhysicsDataCallback& );

   ///Set the bullet rigid for this callback
   ///\param transform bullet rigid body
   void SetbtRigidBody( btRigidBody* transform );

   ///Operator required by osg
   virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

protected:
   btRigidBody* btBody;///<Pointer to the bullet body

};
#elif _OPENSG
#endif
}

#endif //DCS_H
