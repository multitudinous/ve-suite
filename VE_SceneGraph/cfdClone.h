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
 * File:          $RCSfile: cfdClone.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_CLONE_H
#define CFD_CLONE_H
#include "VE_SceneGraph/cfdNode.h"
namespace VE_SceneGraph{
   class cfdDCS;
}
//class cfdMaterial;
namespace VE_SceneGraph{
   class VE_SCENEGRAPH_EXPORTS cfdClone: public cfdNode
   {
      public:
         cfdClone();
         cfdClone(cfdNode* original);
         virtual ~cfdClone();

         void CloneNode(cfdNode* original);
         void SetTranslationArray(float* translation);
         void SetRotationArray(float* rotation);
         void SetScaleArray(float* scale);
         /*void SetMaterial(cfdMaterial* mat);
         void SetDiffuse(float* color);    
         void SetAmbient(float* color);    
         void SetEmmision(float* color);    
         void SetSpecular(float spec);    
         void SetOpacity(float op);
         */

         //returns the cloned structure including the
         //transform
         cfdDCS* GetClonedGraph();
      protected:
         cfdDCS* _cloneTransform;
#ifdef _OSG
         osg::ref_ptr<osg::Node> _originalNode;
         osg::ref_ptr<osg::Node> _instanceNode;
#elif _PERFORMER
        pfNode* _originalNode;
         pfNode* _instanceNode;
#elif _OPENSG
#endif
      //implement later
      /*cfdMaterial* _cloneMaterial*/
   };
}
#endif //CFD_CLONE_H
