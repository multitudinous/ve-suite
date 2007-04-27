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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VOLUME_VISUALIZATION_H
#define CFD_VOLUME_VISUALIZATION_H
/*!\file cfdVolumeVisualization.h
* cfdVolumeVisualization API
*/

/*!\class VE_TextureBased::cfdVolumeVisualization
*
*/
#ifdef VE_PATENTED

#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
namespace osg
{
   class Node;
   class Geometry;
   class Texture1D;
   class Texture3D;
   class TexGen;
   class TexEnv;
   class Geode;
   class ClipNode;
   class TexGenNode;
   class Material;
   class Shape;
   class Image;
   class Switch;
   class StateSet;
   class Group;
   class BoundingBox;
   class Billboard;
}
#include <osgUtil/CullVisitor>
#include <osg/TexMat>
#include <osg/Vec3>

namespace VE_TextureBased
{
   class cfdTextureMatrixCallback;
   class cfdVolumeCenterCallback;
   class cfdVolumeBillboard;
   class cfdTextureManager;
   class TextureBasedVolumeSlices;
}


#include "VE_Xplorer/TextureBased/cfdUpdateTextureCallback.h"
#include "VE_Xplorer/TextureBased/cfdUpdateableOSGTexture1d.h"

#include "VE_Installer/include/VEConfig.h"
#include <string>
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdVolumeVisualization{
   public:
      cfdVolumeVisualization();
      cfdVolumeVisualization(const cfdVolumeVisualization&);
      virtual ~cfdVolumeVisualization();
      enum CfdTexUnit{PLAIN = 0,TRANS_1,TRANS_2,
                 TRANS_3,TRANS_4,PROPERTY,VELOCITY,NOISE};
      enum VisMode{PLAY,STOP};
      enum Direction{FORWARD,BACKWARD};
      enum ClipPlane{XPLANE_MIN=0,
                   XPLANE_MAX,
                   YPLANE_MIN,
                   YPLANE_MAX,
                   ZPLANE_MIN,
                   ZPLANE_MAX,
                   ARBITRARY};

      void SetPlayDirection(Direction dir);
      void SetPlayMode(VisMode mode);
      void SetSliceAlpha(float alpha = .5);
      void SetVeboseFlag(bool flag);
      void SetShaderDirectory(std::string shadDir);
#ifdef _OSG
      void TranslateCenterBy(float* translate);
      void SetStateSet(osg::StateSet* ss);
      void SetState(osg::State* state);
      void Set3DTextureData(osg::Texture3D* texture);
      void SetBoundingBox(float* bbox);
      void SetNumberOfSlices(unsigned int nSlices = 100);
      void SetTextureManager(cfdTextureManager* tm);
      void SetCurrentTransientTexture(unsigned int ct);
      void DisableShaders();
      void CreateNode();
      void AddClipPlane(ClipPlane direction,double* position);
      void RemoveClipPlane(ClipPlane direction);
      void UpdateClipPlanePosition(ClipPlane direction,double* newPosition);
      void ResetClipPlanes();   

      bool isCreated(){return _isCreated;}
      unsigned int GetCurrentTransientTexture();
      cfdUpdateTextureCallback* GetUpdateCallback(){return _utCbk.get();}
      osg::Vec3f GetBBoxCenter(){return _center;}
      float* GetTextureScale(){return _scale;}
      osg::ref_ptr<osg::StateSet> GetStateSet();
      osg::ref_ptr<osg::Texture3D> GetTextureData();
      osg::ref_ptr<osg::Switch> GetVolumeVisNode();
      osg::ref_ptr<osg::Group> GetDecoratorAttachNode();
      VE_TextureBased::TextureBasedVolumeSlices* GetGeometryProxyNode(){return _slices.get();}
   
      cfdVolumeVisualization& operator=(const cfdVolumeVisualization& rhs);
#endif
   protected:
      VisMode _mode;
      Direction _traverseDirection;
      bool _verbose;
      bool _isCreated;
      bool _useShaders;
      bool _volShaderIsActive;
      bool _transferShaderIsActive;
      unsigned int _nSlices;
      unsigned int _tUnit;
      float _alpha;
      void _createVisualBBox();
      void _createClipCube();
      void _buildGraph();
      void _createClipNode();
      void _createStateSet();
      void _attachTextureToStateSet(osg::StateSet* ss);
      void _createTexGenNode();
      void _createVolumeSlices();
      void _buildAxisDependentGeometry();
      void _buildSlices();

      std::string _shaderDirectory;
      cfdTextureManager* _tm;
      osg::Vec3 _center;
      float _transRatio[3];
      float _diagonal;
      float _scale[3];
      float* _vtkBBox;
#ifdef _OSG
      osg::ref_ptr<osg::Switch> _volumeVizNode;
      osg::ref_ptr<osg::TexGenNode> _texGenParams;
      osg::BoundingBox* _bbox;
      osg::ref_ptr<osg::ClipNode> _clipNode;
      osg::ref_ptr<osg::StateSet> _stateSet;
      osg::ref_ptr<VE_TextureBased::TextureBasedVolumeSlices> _slices;
      osg::ref_ptr<osg::Geode> _billboard;
      //osg::ref_ptr<osg::Billboard> _billboard;
      osg::ref_ptr<osg::Group> _noShaderGroup;
      osg::ref_ptr<osg::Group> _decoratorAttachNode;
      osg::ref_ptr<osg::Texture3D> _texture;
      osg::ref_ptr<osg::Image> _image;
      osg::ref_ptr<osg::State> _state;
      osg::ref_ptr<cfdUpdateTextureCallback> _utCbk;
      osg::ref_ptr<cfdVolumeCenterCallback> _vcCbk;
#endif
   };
}
#endif//OSG
#endif// CFD_VOLUME_VISUALIZATION_H
#endif
