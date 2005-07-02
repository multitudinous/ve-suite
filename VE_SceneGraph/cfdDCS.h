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
 * File:          $RCSfile: cfdDCS.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_DCS_H
#define CFD_DCS_H

#include "VE_SceneGraph/cfdGroup.h"

#include <gmtl/Matrix.h>
#include <vector>
class cfdNode;

#ifdef _PERFORMER
class pfDCS;
#elif _OSG
namespace osg
{
   class MatrixTransform;
   class NodeVisitor;
}
#include  <osg/NodeCallback>
#include  <osg/Quat>
#elif _OPENSG
#endif
namespace VE_SceneGraph
{
   class VE_SCENEGRAPH_EXPORTS cfdDCS: public cfdGroup
   {
      public:
         cfdDCS( float*, float*, float* );
      
         cfdDCS( const cfdDCS& );
         cfdDCS& operator= ( const cfdDCS& );

         cfdDCS( void );
         ~cfdDCS( void );

         float* GetVRJTranslationArray( void );
         float* GetVETranslationArray( void );
         float* GetRotationArray( void );
         float* GetScaleArray( void );

         void SetTranslationArray( float* );
         void SetRotationArray( float* );
         void SetScaleArray( float* );
         void SetRotationMatrix( gmtl::Matrix44f& );

         gmtl::Matrix44f GetMat( void );
         void SetMat( gmtl::Matrix44f& );

         //need to override these!!!!
         int RemoveChild(cfdNode* child);
         int AddChild(cfdNode* child);
         void InsertChild(int index,cfdNode* child);
         int GetNumChildren();
         const char* GetName();
         void SetName(char* name);
         int ReplaceChild(cfdNode* oldChild, cfdNode* newChild);


#ifdef _PERFORMER
         pfNode* GetRawNode( void );
#elif _OSG
         osg::Node* GetRawNode( void );
      class cfdUpdateDCSCallback : public osg::NodeCallback
      {
         public:
            cfdUpdateDCSCallback();
            virtual ~cfdUpdateDCSCallback(){;}
            cfdUpdateDCSCallback( const cfdUpdateDCSCallback& );
            void setRotationDegreeAngles(float h,float p,float r);
            void setTranslation(float* trans);
            void setScaleValues(float* scale);
            void setQuat( osg::Quat& );
            virtual void operator()(osg::Node* node, osg::NodeVisitor* nv);
         protected:
            float _scale[3];
            float _trans[3];
            float _h;
            float _p;
            float _r;
            osg::Quat quat;
      };
#elif _OPENSG
#endif

      private:
#ifdef _PERFORMER
         pfDCS* _dcs;
#elif _OSG
         osg::ref_ptr<osg::MatrixTransform> _dcs;
         cfdUpdateDCSCallback* _udcb;
         osg::Quat dcsQuat;
#elif _OPENSG
#endif

         float _translation[ 3 ];
         float vrjTranslation[ 3 ];
         float _rotation[ 3 ];
         float _scale[ 3 ];

         gmtl::Matrix44f _vjMatrix;
    };
}
#endif
