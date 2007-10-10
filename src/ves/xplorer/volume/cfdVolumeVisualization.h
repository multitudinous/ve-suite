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
#ifndef CFD_VOLUME_VISUALIZATION_H
#define CFD_VOLUME_VISUALIZATION_H
/*!\file cfdVolumeVisualization.h
* cfdVolumeVisualization API
*/

/*!\class VE_TextureBased::cfdVolumeVisualization
*
*/
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
   class PositionAttitudeTransform;
}
#include <osgUtil/CullVisitor>
#include <osg/TexMat>
#include <osg/Vec3>

namespace VE_TextureBased
{
   class cfdTextureMatrixCallback;
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
      ///Constructor
      cfdVolumeVisualization();
      ///Copy constructor
      ///\param volumeVizNode The node to copy
      cfdVolumeVisualization(const cfdVolumeVisualization& volumeVizNode);
      ///Destructor
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

      ///Set the step direction
      ///\param dir Direction to step
      void SetPlayDirection(Direction dir);
      ///\param mode Play Mode for the cfdTextureManager
      void SetPlayMode(VisMode mode);
      ///DEPRICATED\n Set the alpha for each slice     
      ///\param alpha Slice alpha
      void SetSliceAlpha(float alpha = .5);
      ///Turn on debug output
      ///\param flag Debug output
      void SetVeboseFlag(bool flag);
      ///DEPRICATED\n Set the directory to load the shaders from
      ///\param shadDir The directory containing the shaders to load.
      void SetShaderDirectory(std::string shadDir);
#ifdef _OSG
      ///Translate the center of volume
      ///DEPRICATED\n
      ///\param translate Translation value
      void TranslateCenterBy(float* translate);
      ///Set the state set
      ///DEPRICATED\n
      ///\param ss The osg::StateSet
      void SetStateSet(osg::StateSet* ss);
      ///Set the gl state before rendering this node
      ///DEPRICATED\n
      ///\param state The osg::State
      void SetState(osg::State* state);
      ///Set the texture data
      ///DEPRICATED\n
      ///\param texture The osg::Texture3D
      void Set3DTextureData(osg::Texture3D* texture);
      ///Set the bounding box for the data
      ///\param bbox The bounding box in VTK format, ie xmin,ymin,zmin,xmax,ymax,zmax
      void SetBoundingBox(float* bbox);

      ///Set the number of slicing polygons
      ///\param nSlice The number of slices
      void SetNumberOfSlices(unsigned int nSlices = 100);
      ///Set the texture data
      ///\param tm The cfdTextureManager
      void SetTextureManager(cfdTextureManager* tm);
      ///Set the position on the cfdTextureManager
      ///\param ct Position of the cfdTextureManager
      void SetCurrentTransientTexture(unsigned int ct);
      ///Disable use of shaders
      void DisableShaders();
      ///Create the violume visualzation node
      void CreateNode();
      ///Add a volume clipping plane
      ///\param direction The axis normal to the clip plane
      ///\param position The position along the axis
      void AddClipPlane(ClipPlane direction,double* position);
      ///Remove the clip plane specified by direction
      void RemoveClipPlane(ClipPlane direction);
      ///Update the clip  plane position
      ///\param direction The axis normal to the clip plane
      ///\param position The position along the axis
      void UpdateClipPlanePosition(ClipPlane direction,double* newPosition);
      
      ///Reset all the clip planes to original positions
      void ResetClipPlanes();   

      ///Check if the noed is initialized
      bool isCreated(){return _isCreated;}
      ///Get the current time step
      unsigned int GetCurrentTransientTexture();
      ///The texture update callback
      cfdUpdateTextureCallback* GetUpdateCallback(){return _utCbk.get();}
      ///The center of the bbounding box
      osg::Vec3f GetBBoxCenter(){return _center;}
      ///Get the texture scale
      float* GetTextureScale(){return _scale;}
      ///Get the state set
      osg::ref_ptr<osg::StateSet> GetStateSet();
      ///Get the texture data
      osg::ref_ptr<osg::Texture3D> GetTextureData();
      ///Get the top level node
      osg::ref_ptr<osg::Switch> GetVolumeVisNode();
      ///Get the node to attach shaders to
      osg::ref_ptr<osg::Group> GetDecoratorAttachNode();
      ///Get the proxy geometry
      VE_TextureBased::TextureBasedVolumeSlices* GetGeometryProxyNode(){return _slices.get();}
   
      ///Copy constructor
      ///\param rhs The cfdVolumeVisualization to set equal to
      cfdVolumeVisualization& operator=(const cfdVolumeVisualization& rhs);
#endif
   protected:
      VisMode _mode;///<Play mode for the texture manager
      Direction _traverseDirection;///<Direction to step for the cfdTextureManager
      bool _verbose;///<Debug messages
      bool _isCreated;///<Graph is initialized
      bool _useShaders;///<Use shaders for rendering
      bool _volShaderIsActive;///<Volume shader is active
      bool _transferShaderIsActive;///<Transfer shader is active
      unsigned int _nSlices;///<The number of slices
      unsigned int _tUnit;///<The texture unit

      float _alpha;///<Slice transparency
      ///Create the visual bounding box
      void _createVisualBBox();
      ///Create a clipping cube from the bounding box data
      void _createClipCube();
      ///Initialize the tranform for the volume
      void _initializeVolumeDCS();

      ///Build the volume visualization graph
      void _buildGraph();
      ///Create the clip node
      void _createClipNode();
      ///Create the state set
      void _createStateSet();
      ///DEPRICATED\n Attach the texture to the state set
      ///\param ss The osg::StateSet that owns the 3d texture
      void _attachTextureToStateSet(osg::StateSet* ss);
      ///Create the texture coordiante generation node
      void _createTexGenNode();
      ///Ensure that the slicing geometry is created
      void _createVolumeSlices();
      ///DEPRICATED\n Build the axis depenent slices
      void _buildAxisDependentGeometry();
      ///Build the slicing polygon
      void _buildSlices();

      std::string _shaderDirectory;///<DEPRICATED\n The directory of the shaders
      cfdTextureManager* _tm;///The cfd Texture manger
      osg::Vec3f _center;///<The ceneter of of the data
      float _transRatio[3];///<Used for building the texture matrix
      float _diagonal;///<BBox diagonal
      float _scale[3];///<The scale
      float* _vtkBBox;///<VTK formatted bounding box
#ifdef _OSG
      osg::ref_ptr<osg::Switch> _volumeVizNode;///<Top-level node 
      osg::ref_ptr<osg::TexGenNode> _texGenParams;///<The texture coordinate generation parameters
      osg::BoundingBox* _bbox;///< The bounding box of the data
      osg::ref_ptr<osg::ClipNode> _clipNode;///<Clipping nodes for the volume
      osg::ref_ptr<osg::StateSet> _stateSet;///<The default state set
      osg::ref_ptr<VE_TextureBased::TextureBasedVolumeSlices> _slices;///<The volume rendering slices
      osg::ref_ptr<osg::PositionAttitudeTransform> _volumeDCS;///<The transform for the volume rendering
      osg::ref_ptr<osg::Geode> _billboard;///<The geode holding the slice
      osg::ref_ptr<osg::Group> _noShaderGroup;///<The default group for volume rendering
      osg::ref_ptr<osg::Group> _decoratorAttachNode;///<Node to attach shaders to
      osg::ref_ptr<osg::Texture3D> _texture;;///<DEPRICATED\n3D texture.
      osg::ref_ptr<osg::Image> _image;///<DEPRICATED\n Image data
      osg::ref_ptr<osg::State> _state;///<DEPRICATED\nGL state
      osg::ref_ptr<cfdUpdateTextureCallback> _utCbk;///<Texture update callback
#endif
   };
}
#endif//OSG
#endif// CFD_VOLUME_VISUALIZATION_H
