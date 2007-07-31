
/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _BIV_TEXTURE_MANAGER_H_
#define _BIV_TEXTURE_MANAGER_H_
/*!\file cfdTextureManager.h
* cfdTextureManager API
*/

/*!\class VE_TextureBased::cfdTextureManager
*
*/
#ifdef WIN32
#include <windows.h>
#endif

//#include <gl/gl.h>
#include <iostream>
#include <vector>
#include <string>
struct ScalarRange
{
   float range[2];
};
#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
class  VE_TEXTURE_BASED_EXPORTS cfdTextureManager{
public:
   ///Constructor
   cfdTextureManager();
   ///Copy Constructor
   ///\param tm The cfdTextureManager to create a copy from.
   cfdTextureManager(const cfdTextureManager& tm);

   ///Destructor
   virtual ~cfdTextureManager();

   enum DataType{SCALAR,VECTOR};
   enum PlayMode{PLAY,STOP,STEP};

   ///Add texture data from a vti file
   ///\param textureFile Path (including name) to the vti file 
   void addFieldTextureFromFile(std::string textureFile);

   ///Set the play mode via string
   ///"Play","Stop"
   ///\param mode The play mode to set active
   void SetPlayMode(std::string mode);

   ///DEPRICATED.\n Set the play mode via enum
   ///\param mode The play mode to set active
   void setPlayMode(PlayMode mode){_mode = mode;}
   ///Set the direction to step through transient texture data.\n
   ///forwardBackward == -1 step backward\n
   ///forwardBackward == 1 step forward\n
   ///\param forwardBackward The direction to sequence through data.
   void setDirection(int forwardBackward);

   ///Set the current "frame", ie timestep within the texture sequence.
   ///\param whichFrame The frame, ie timestep to set as current.
   ///\param isOnSlave The app is running on a slave node of the cluster.
   void SetCurrentFrame(unsigned int whichFrame, bool isOnSlave=true);

   ///Set the flag for using shaders
   ///\param useShaders true/false 
   void SetUseShaders(bool useShaders);

   ///Determine if shaders are being used
   bool UseShaders(){return _useShaders;}

   /// Check if it is time to advance the sequence
   bool TimeToUpdate();

   ///Get the real-world bounds of the data
   float* getBoundingBox(){return _bbox;}
   ///Determine if it is time to step the data based on currentTime - lastUpdateTime > delay
   ///NOTE: This requires VRJ Start Barrier to work properly
   ///\param currentTime The current time from the application.
   ///\param delay The amount of time between each timestep
   void CalculateUpdateTime(double curTime,double delay);

   ///Get the texture data at a given timestep
   ///\param timeStep The timestep to retrieve texture data
   unsigned char* dataField(int timeStep){return _dataFields.at(timeStep);}

   ///Get the next texture data field
   unsigned char* getNextField();

   ///Get the current texture data field
   unsigned char* getCurrentField();
   
   ///Get the index of the next texture data field
   unsigned int getNextFrame();

   ///Get the number of texture data fields
   int numberOfFields(){return static_cast< int >( _dataFields.size() );}

   ///The resolution (s,t,r, dimension)of the fields
   int* fieldResolution(){return _resolution;}

   ///The index of the current frame
   unsigned int GetCurrentFrame();
   
   ///The current "play mode"
   PlayMode getPlayMode(){return _mode;}
   
   ///Return data scalar range for specified time step
   ///\param index The timestep to retrieve the scalar range for
   ScalarRange dataRange(int index){return _ranges.at(index);}

   ///The scalar range for the entire sequence
   float* transientRange(){return _transientRange;}

   ///The type of data contained in this texture manager\n
   ///cfdTextureMangers contain either scalar or vector data but not a mix of both
   ///\param whichField The index to retrieve the type from
   DataType GetDataType(int whichField){return _types.at(whichField);}

   ///Equal operator
   ///\param tm The cfdTextureManager to set equal
   cfdTextureManager& operator=(const cfdTextureManager& tm);

   ///Get the name of this data 
   std::string GetDataName( void ); 

   ///Query if app is running on a slave node in the cluster
   bool IsOnSlaveNode();
protected:
   ///calculate the length of the diagonal of the bbox
   ///\param bbox the bbox parameters
   double _lengthOfBBox(float* bbox);

   ///Make sure we have the largest bbox
   ///\param bbox the bbox parameters
   void _ensureBBox(double* bbox);

   bool m_isSlave;///<Is this cfdTextureManager running on a slave node.
   bool _useShaders;///<Flag for shaders
   bool _timeToUpdate;///<Flag deteriming update time
   int _curField;///<The current field
   int* _resolution;///<The resolution, (dimensions in s,t,r)
   std::vector<DataType> _types;///<List of types
   float _bbox[6];///<Bounding box of the data
   float _range[2];///<Intermediate range
   float _transientRange[2];///<Total transient scalar range

   std::vector<ScalarRange> _ranges;///< Individual ranges
   std::vector<unsigned char*> _dataFields;///<The raw data for each timestep
   double _prevTime;///<The last time the field was updated
   int _direction;///<Step direction
   PlayMode _mode;///<Play mode
   std::string dataName;///<Name of this data solution.
};
}
#endif //_BIV_TEXTURE_MANAGER_H_
