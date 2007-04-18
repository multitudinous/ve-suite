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
 * Date modified: $Date: 2007-04-05 15:23:00 -0500 (Thu, 05 Apr 2007) $
 * Version:       $Rev: 7270 $
 * Author:        $Author: biv $
 * Id:            $Id: cfdVolumeVisualization.h 7270 2007-04-05 20:23:00Z biv $
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TRANSFER_FUNCTION_H
#define TRANSFER_FUNCTION_H
#include <map>
#include <vector>
/*!\file TransferFunction.h
  Transfer function API
  */
/*!\class VE_TextureBased::TransferFunction
 * Abstract class defining a transfer function.
 */
#include "VE_Installer/include/VEConfig.h"
namespace VE_TextureBased
{
class VE_TEXTURE_BASED_EXPORTS TransferFunction
{
public:
   enum ComponentType{
                      LINEAR,
                      RAMP,
	                   STEP,
	                   CUBIC,
	                   QUAD,
	                   EXPONENTIAL,
	                   CUSTOM
                      };
   ///Constructor
   ///\param dimension 1-3 are valid values.
   ///\param s Power of 2 (ie, 2^n) resolution
   ///\param t Power of 2 (ie, 2^n) resolution
   ///\param r Power of 2 (ie, 2^n) resolution
   TransferFunction(unsigned int dimension=1,
                        unsigned int s=256,
		                unsigned int t = 1,
		                unsigned int r = 1);
   
   ///Copy constructor
   TransferFunction(const TransferFunction& rhs);

   ///Destructor
   virtual ~TransferFunction();

   ///Initialize the data in the function
   virtual void InitializeData()=0;

   ///Set the components general "shape"
   ///\param type The ComponentType
   ///\param component The ComponentType\n 0 == "RED"\n1 == "GREEN"\n2 == "BLUE"\n3 == "ALPHA"
   void SetComponentType(ComponentType type, unsigned int component);

   ///Set the scalar range
   ///\param minValue Minimum scalar value
   ///\param maxValue Maximum scalar value
   void SetFullScalarRange(float minValue,float maxValue);

   ///Adjust the minimum value of the scalar range
   ///\param minValue The new minimum value. \n This is the real value NOT the percentage of original.
   void AdjustScalarMinimum(float minValue);
   
   ///Adjust the maximum value of the scalar range
   ///\param maxValue The new maximum value. \n This is the real value NOT the percentage of original.
   void AdjustScalarMaximum(float maxValue);

   ///Flag specifying if the transfer function is an isosurface
   ///\param isIsosurface Isosurface flag
   void SetIsoSurface(bool isIsosurface);

   ///Set the isosurface value as a percentage of the current range.
   ///\param percentage The isovalue as a percentage of the current range
   void SetIsoSurfaceValue(float percentage);

   ///Update the transfer function
   ///\param component The ComponentType\n 0 == "RED"\n1 == "GREEN"\n2 == "BLUE"\n3 == "ALPHA"
   ///\param data A pointer for passing in user data
   ///\param updateRangeMin The starting value to update
   ///\param updateRangeMax The ending value to update
  /* virtual void Update(unsigned int component,
                            void* data,
                            float updateRangeMin=0.0,
                            float updateRangeMax=1.0)=0;*/

   ///Get the resolution in a particular direction (r,s,t)
   ///\param direction 0 == s\n 1 == t\n 2 == r\n
   unsigned int GetResolution(unsigned int direction);

   ///Get the general "shape" for a specific component
   ///\param component The ComponentType\n 0 == "RED"\n1 == "GREEN"\n2 == "BLUE"\n3 == "ALPHA"
   ComponentType GetComponentType(unsigned int component);

   ///Evaluate the transfer function
   ///\param index The input to the transfer function
   float* EvaluateAt(unsigned int index);

   ///Get the dimension of the transfer function\n
   unsigned int GetDimension();

   ///Get the unsigned char data
   unsigned char* GetDataForTexture();

   ///Get the texture data
  inline unsigned char& unsignedByteDataAt (int i)
  { 
	  return _textureData[i]; 
  }
   ///Equal operator
   ///\param rhs The TransferFunction to set this one to. 
   TransferFunction& operator=(const TransferFunction& rhs);

   class UpdateCallback
   {
   public:
      UpdateCallback(TransferFunction* tf){_tf = tf;}
      virtual ~UpdateCallback(){}
      virtual void UpdateData()=0;
   protected:
      TransferFunction* _tf;
   };

   ///Set the transfer function update callback
   ///\param tfUpdate Set the update callback
   void SetUpdateCallback(UpdateCallback* tfUpdate);

   ///Get the update callback
   UpdateCallback* GetUpdateCallback();
protected:

	///Update the transfer function
	virtual void _update()=0;
   ///Set the resolution (power of 2) of the TF in a particular dimension
   ///\param direction "S","T","R" 
   ///\param resolution Power of 2 (ie, 2^n) resolution
   void _setResolution(unsigned int direction,
                       unsigned int resolution);
   ///Set the dimension of the transfer function\n
   ///\note Once created the dimensions CANNOT be modified.\n This is to protect from corrupting GL because this data is used for textures
   ///\param dimension 1-3 are valid values.
   void _setDimension(unsigned int dimension);

   bool _isoSurface;///<Flag for isosurfaces.
   float _percentIsoValue;///<Percentage for the isovalue calculation.
   unsigned int _dimension;///<The dimension of the transfer function
   unsigned int _resolution[3];///<The resolution in s,t,r directions.
   ComponentType _types[4];///<The RGBA component Types 

   float* _classification;///<The lookup values for RGBA
   unsigned char* _textureData;///<The texture data for representing the TF
   float _originalScalarRange[2];///<The full avaliable scalar range.
   float _currentScalarRange[2];///<The current scalar range;
   UpdateCallback* _updateCallback;///<Callback function to update the data in the TF
};
#define TransferFunction1D(X) TransferFunction(1,X,1,1)
}
#endif// TRANSFER_FUNCTION_H
