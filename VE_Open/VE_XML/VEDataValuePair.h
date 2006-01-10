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
 * File:          $RCSfile: VEDataValuePair.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_DATA_VALUE_PAIR_H_
#define _XML_VE_DATA_VALUE_PAIR_H_

#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
   class VETransform;
   class VEFloatArray;
}

namespace VE_XML
{
class VE_XML_EXPORTS VEDataValuePair : public VEXMLObject
{
public:
   
   //valid values are 
   //STRING == a string value
   //FLOAT == a single float value
   //FARRAY == a float array
   //TRANSFORM == a vetransform
   VEDataValuePair(DOMDocument* rootDoc,std::string type=std::string("STRING"));
   virtual ~VEDataValuePair();
   VEDataValuePair( const VEDataValuePair& );
   //equal operator
   VEDataValuePair& operator= ( const VEDataValuePair& );
   
   // This is the name of the datavaluepair that is encoded on the xml data type
   void SetDataName(std::string name);

   //Type
   void SetDataType(std::string type);

   //the different data types
   //string
   void SetDataString(std::string data);
   
   //float
   void SetDataValue(float data);
   
   //float array
   void SetDataArray( VE_XML::VEFloatArray* );

   //a VETransform contains the parameter necesary to
   //construct a transformation matrix.
   //ie. 3 float arrays: translation,scale,rotation
   void SetDataTransform(VE_XML::VETransform* xForm);

   //set the data from an string representing the xml
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   std::string GetDataType();
   std::string GetDataName();

   std::string GetDataString();
   double GetDataValue();
   VE_XML::VEFloatArray* GetDataArray( void );
   VE_XML::VETransform* GetDataTransform( void );

protected:
   virtual void _updateVEElement( std::string );
   void _updateDataName();
   void _updateDataValueNumber();
   void _updateDataValueString();
   std::string _dataType;
   std::string _dataName;

   // raw datatypes of datavaluepair that are specified in the verg.xsd file
   double _dataValue;
   std::string _dataString;
   VE_XML::VEFloatArray* _dataArray;
   VE_XML::VETransform* _dataTransform;
};
}
#endif// _VE_DATA_VALUE_PAIR_H_
