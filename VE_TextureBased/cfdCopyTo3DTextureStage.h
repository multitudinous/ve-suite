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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_COPY_TO_3DTEXTURE_STAGE_H
#define CFD_COPY_TO_3DTEXTURE_STAGE_H
#ifdef VE_PATENTED
#ifdef _OSG

#include <osg/Texture3D>
#include <osg/State>
#include <osg/StateSet>
#include <osgUtil/RenderStage>
#include <osg/FrameStamp>
#include "VE_TextureBased/cfdPBufferManager.h"
#include "VE_Installer/include/VEConfig.h"
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdCopyTo3DTextureStage:
      public osgUtil::RenderStage
   {
      public:
         cfdCopyTo3DTextureStage();
         virtual ~cfdCopyTo3DTextureStage();
         virtual osg::Object* cloneType() const { return new cfdCopyTo3DTextureStage(); }
         virtual osg::Object* clone(const osg::CopyOp&) const { return new cfdCopyTo3DTextureStage(); } // note only implements a clone of type.
         virtual bool isSameKindAs(const osg::Object* obj) const { return dynamic_cast<const cfdCopyTo3DTextureStage*>(obj)!=0L; }
         virtual const char* libraryName() const { return ""; }
         virtual const char* className() const { return "cfdCopyTo3DTextureStage"; }

         inline void setPBuffer(cfdPBufferManager* pbuffer) { _pbuffer = pbuffer; }
  
         void SetWhichSliceToUpdate(unsigned int whichSlice);

         //whichSlice corresponds to offset in the 3d texture
         //whichDir corresponds to the direction (x =0,y=1,z=2)
         inline void setSliceToUpdate(int whichSlice=0,int whichDir=2)
         {
            _whichSlice = whichSlice;
            _whichDir = whichDir;
         }
         void SetViewDirection(int whichDir=2){_whichDir = whichDir;}
         virtual void reset();
        
         void set3DTexture(osg::Texture3D* texture) { _texture = texture; }
         void SetShaderStateSet(osg::StateSet* ss);
         osg::Texture3D* getTexture() { return _texture.get(); }

         virtual void draw(osg::State& state,osgUtil::RenderLeaf*& previous);

      protected:   
  
         osg::ref_ptr<osg::FrameStamp> _fs;
         osg::ref_ptr<osg::Texture3D> _texture;
         osg::ref_ptr<osg::State> _localState;
         osg::ref_ptr<osg::StateSet> _shader;
         cfdPBufferManager* _pbuffer;

         unsigned int _whichSlice;
         unsigned int _whichDir;
         int _width;
         int _height;
        int _nSlices;
   };
}
#endif //OSG
#endif
#endif// CFD_COPY_TO_3DTEXTURE_STAGE_H

