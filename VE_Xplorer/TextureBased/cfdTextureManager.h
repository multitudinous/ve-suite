
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
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
#ifdef VE_PATENTED
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
   cfdTextureManager();
   cfdTextureManager(const cfdTextureManager& tm);
   ~cfdTextureManager();

   enum DataType{SCALAR,VECTOR};
   enum PlayMode{PLAY,STOP,STEP};

   //add a vector field from a file
   void addFieldTextureFromFile(std::string textureFile);

   ///Set the play mode via string
   ///"Play","Stop"
   void SetPlayMode(std::string mode);

   void setPlayMode(PlayMode mode){_mode = mode;}
   //forwardBackward == -1 backward
   //forwardBackward == 1 forward
   void setDirection(int forwardBackward);

   //set the current frame
   void SetCurrentFrame(unsigned int whichFrame);

   //set the flag for using shaders
   void SetUseShaders(bool useShaders);

   //get the flag
   bool UseShaders(){return _useShaders;}

   /// check for updating
   bool TimeToUpdate();

   float* getBoundingBox(){return _bbox;}
   void CalculateUpdateTime(double curTime,double delay);

   //get the vector field at a given timestep
   unsigned char* dataField(int timeStep){return _dataFields.at(timeStep);}

   //get the next vector field
   unsigned char* getNextField();
   unsigned char* getCurrentField();
   unsigned int getNextFrame();

   //get the number of vector fields
   int numberOfFields(){return static_cast< int >( _dataFields.size() );}

   //the resolution of the fields
   int* fieldResolution(){return _resolution;}

   //the current frame
   unsigned int GetCurrentFrame();

   PlayMode getPlayMode(){return _mode;}
   //the data ranges
   ScalarRange dataRange(int index){return _ranges.at(index);}

   float* transientRange(){return _transientRange;}

   DataType GetDataType(int whichField){return _types.at(whichField);}

   //equal operator
   cfdTextureManager& operator=(const cfdTextureManager& tm);

   std::string GetDataName( void ); 
protected:
   ///calculate the length of the diagonal of the bbox
   ///\param bbox the bbox parameters
   double _lengthOfBBox(float* bbox);

   ///Make sure we have the largest bbox
   ///\param bbox the bbox parameters
   void _ensureBBox(double* bbox);

   bool _useShaders;
   bool _timeToUpdate;
   int _curField;
   int* _resolution;
   std::vector<DataType> _types;
   float _bbox[6];
   float _range[2];
   float _transientRange[2];

   std::vector<ScalarRange> _ranges;
   std::vector<unsigned char*> _dataFields;
   double _prevTime;
   int _direction;
   PlayMode _mode;
   std::string dataName;
};
}
#endif
#endif //_BIV_TEXTURE_MANAGER_H_
