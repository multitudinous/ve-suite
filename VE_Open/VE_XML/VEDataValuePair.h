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
 * File:          $RCSfile: VEDataValuePair.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_DATA_VALUE_PAIR_H_
#define _XML_VE_DATA_VALUE_PAIR_H_
/*!\file VEDataValuePair.h
  *Data value pairs API
  */

/*!\class VE_XML::VEDataValuePair
 *Class that manages data value pairs.
 *Pairs consist of a name and a value which can be of types:
 *float,float array,string and transform
 */
#include <string>
#include <vector>
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
   ///Constructor
   ///\param rootDoc The owning DOMDocument
   ///\param type The type of value in this pair.
   VEDataValuePair(DOMDocument* rootDoc,std::string type=std::string("STRING"));
   ///Destructor
   virtual ~VEDataValuePair();
   ///Copy Constructor
   VEDataValuePair( const VEDataValuePair& );
   ///equal operator
   VEDataValuePair& operator= ( const VEDataValuePair& );
   
   /// Set the name 
   ///\param name The name of this data value pair
   void SetDataName(std::string name);

   ///Set the data value type
   ///\param type Set the type of the data held.
   void SetDataType(std::string type);

   ///Set the string data
   ///\param data The string data.
   void SetDataString(std::string data);
   
   ///Set the float data
   ///\param data The float data.
   void SetDataValue(float data);
   
   
   ///Set the VEFloatArray data
   ///\param data The VEFloatArray data.
   void SetDataArray( VE_XML::VEFloatArray* data );

   ///Set the VETransform data.
   ///\param xForm The VETransform data.
   void SetDataTransform(VE_XML::VETransform* xForm);

   ///set the data from an string representing the xml
   ///\param xmlInput The input XML data.
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   ///Get the data type.
   std::string GetDataType();
   ///Get the data name.
   std::string GetDataName();

   ///Get the string data.
   std::string GetDataString();
   
   ///Get the value of the double data.
   double GetDataValue();

   ///Get the VEFloatArray data.
   VE_XML::VEFloatArray* GetDataArray( void );
   
   ///Get the VETransform data.
   VE_XML::VETransform* GetDataTransform( void );

protected:
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName );

   ///Internally update the data name.
   void _updateDataName();
   ///Internally update the data value number.
   void _updateDataValueNumber();

   ///Internally update the data value string.
   void _updateDataValueString();

   std::string _dataType;///<The data type.
   std::string _dataName;///<The data name.

   ///raw datatypes of VEDataValuePair that are specified in the verg.xsd file
   double _dataValue;///<Raw double value.
   std::string _dataString;///<Raw string value.
   VE_XML::VEFloatArray* _dataArray;///<Raw VEFloatArray value.
   VE_XML::VETransform* _dataTransform;///<Raw VETransform value.
};
}
#endif// _VE_DATA_VALUE_PAIR_H_
