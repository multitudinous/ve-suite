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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/cfdDCS.h"
#include "VE_Xplorer/SceneGraph/cfdClone.h"
#ifdef _OSG
#include <osg/MatrixTransform>
#include <osg/CopyOp>
//#include "VE_SceneGraph/cfdMaterial.h"
#elif _PERFORMER
#elif _OPENSG
#endif
using namespace VE_SceneGraph;
////////////////////
cfdClone::cfdClone()
:cfdNode()
{
   _cloneTransform = 0;
   //_originalNode = 0;
   //_instanceNode = 0;
   //implement later
   /*_cloneMaterial = 0*/
}
/////////////////////////////////////
cfdClone::cfdClone(cfdNode* original)
:cfdNode()
{
   _cloneTransform = 0;
   //_originalNode = 0;
   //_instanceNode = 0;
   CloneNode( original );
}
/////////////////////
cfdClone::~cfdClone()
{
   if(_cloneTransform){
      delete _cloneTransform;
      _cloneTransform = 0;
   }
}
///////////////////////////////////////////
void cfdClone::CloneNode(cfdNode* original)
{
   if(!_cloneTransform){
      _cloneTransform = new cfdDCS();
   }
   cfdDCS* assemblyNode = dynamic_cast<cfdDCS*>(original);
   if(assemblyNode)
   {
      for(unsigned int i =0; i < assemblyNode->GetNumChildren(); i ++)
      {
         _cloneTransform->AddChild(assemblyNode->GetChild(i));
      }

   }
   else
   {
      _cloneTransform->AddChild(original);
   }
/*#ifdef _OSG
   _originalNode = original->GetRawNode();
   _instanceNode = dynamic_cast<osg::Node*>(_originalNode->clone(osg::CopyOp::SHALLOW_COPY));
   if(_instanceNode.valid())
   {
      unsigned int nChildren = _cloneTransform->GetNumChildren();
      if(nChildren){
         //should only be one child
         _cloneTransform->RemoveChild(_cloneTransform->GetChild(0));
      }
      ((osg::MatrixTransform*)_cloneTransform->GetRawNode())->addChild(_instanceNode.get());
   }
#elif _PERFORMER
#elif _OPENSG
#endif*/
}
/////////////////////////////////////////////////
void cfdClone::SetTranslationArray(float* translation)
{
   if(_cloneTransform)
   {
      _cloneTransform->SetTranslationArray(translation);
   }
}
/////////////////////////////////////////////////
void cfdClone::SetRotationArray(float* rotation)
{
   if(_cloneTransform)
   {
      _cloneTransform->SetRotationArray(rotation);
   }
}
/////////////////////////////////////////////////
void cfdClone::SetScaleArray(float* scale)
{
   if(_cloneTransform)
   {
      _cloneTransform->SetScaleArray(scale);
   }
}
//////////////////////////////////
cfdDCS* cfdClone::GetClonedGraph()
{
   if(_cloneTransform)
   {
      return _cloneTransform;
   }
   return 0;
}
   /*void SetMaterial(cfdMaterial* mat);
     void SetDiffuse(float* color);    
     void SetAmbient(float* color);    
     void SetEmmision(float* color);    
     void SetSpecular(float spec);    
     void SetOpacity(float op);
    */

   //returns the cloned structure including the
   //transform
