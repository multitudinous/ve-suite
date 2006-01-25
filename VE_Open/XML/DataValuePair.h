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
 * File:          $RCSfile: DataValuePair.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_DATA_VALUE_PAIR_H_
#define _XML_VE_DATA_VALUE_PAIR_H_
/*!\file DataValuePair.h
  *Data value pairs API
  */

/*!\class VE_XML::DataValuePair
 *Class that manages data value pairs.
 *Pairs consist of a name and a value which can be of types:
 *float,float array,string and transform
 */
#include <string>
#include <vector>
#include "VE_Open/XML/XMLObject.h"

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
   class Transform;
   class FloatArray;
   class OneDDoubleArray;
   class TwoDDoubleArray;
   class ThreeDDoubleArray;
   class OneDIntArray;
   class TwoDIntArray;
   class ThreeDIntArray;
   class OneDStringArray;
   //class TwoDStringArray;
   //class ThreeDStringArray;
}

namespace VE_XML
{
class VE_XML_EXPORTS DataValuePair : public XMLObject
{
public:
   
   //valid values are 
   //STRING == a string value
   //FLOAT == a single float value
   //FARRAY == a float array
   //TRANSFORM == a Transform
   //1DSTRING
   //1DDOUBLE
   //2DDOUBLE
   //3DDOUBLE
   //LONG
   //1DLONG
   //2DLONG
   //3DLONG
   ///Constructor
   ///\param rootDoc The owning DOMDocument
   ///\param type The type of value in this pair.
   DataValuePair(DOMDocument* rootDoc,std::string type=std::string("STRING"));
   ///Destructor
   virtual ~DataValuePair();
   ///Copy Constructor
   DataValuePair( const DataValuePair& );
   ///equal operator
   DataValuePair& operator= ( const DataValuePair& );
   
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
   void SetDataValue( double data );
   
   
   ///Set the FloatArray data
   ///\param data The FloatArray data.
   void SetDataArray( VE_XML::FloatArray* data );

   ///Set the Transform data.
   ///\param xForm The Transform data.
   void SetDataTransform(VE_XML::Transform* xForm);

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

   ///Get the FloatArray data.
   VE_XML::FloatArray* GetDataArray( void );
   
   ///Get the Transform data.
   VE_XML::Transform* GetDataTransform( void );

   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, VE_XML::FloatArray* data );
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, VE_XML::Transform* data );

   ///String data
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, std::string data );
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, std::vector< std::string > data );
   
   ///Double arrays
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, double data );
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, std::vector< double > data );
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, std::vector< std::vector< double > > data );
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, std::vector< std::vector< std::vector< double > > > data );

   ///Int arrays
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, long data );
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, std::vector< long > data );
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, std::vector< std::vector< long > > data );
   ///Helper functions to set data easily
   ///\param dataName Name of the data being passed in
   ///\param data Data value being passed in. Can be a broad range of data types
   void SetData( std::string dataName, std::vector< std::vector< std::vector< long > > > data );

   ///String data
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( std::string& data );
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( std::vector< std::string >& data );
   
   ///Double arrays
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( double& data );
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( std::vector< double >& data );
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( std::vector< std::vector< double > >& data );
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( std::vector< std::vector< std::vector< double > > >& data );

   ///Int arrays
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( long& data );
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( std::vector< long >& data );
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( std::vector< std::vector< long > >& data );
   ///Helper functions to get data easily
   ///\param data Name of the data being passed in
   void GetData( std::vector< std::vector< std::vector< long > > >& data );

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

   ///raw datatypes of DataValuePair that are specified in the verg.xsd file
   double _dataValue;///<Raw double value.
   long intDataValue;///<Raw long value.
   std::string _dataString;///<Raw string value.
   VE_XML::FloatArray* _dataArray;///<Raw FloatArray value.
   VE_XML::Transform* _dataTransform;///<Raw Transform value.
   VE_XML::OneDDoubleArray* oneDDouble;///<Raw double value.
   VE_XML::TwoDDoubleArray* twoDDouble;///<Raw double value.
   VE_XML::ThreeDDoubleArray* threeDDouble;///<Raw double value.
   VE_XML::OneDIntArray* oneDInt;///<Raw double value.
   VE_XML::TwoDIntArray* twoDInt;///<Raw double value.
   VE_XML::ThreeDIntArray* threeDInt;///<Raw double value.
   VE_XML::OneDStringArray* oneDString;///<Raw double value.
   //VE_XML::TwoDStringArray* twoDString;///<Raw double value.
   //VE_XML::ThreeDStringArray* threeDString;///<Raw double value.
};
}
#endif// _VE_DATA_VALUE_PAIR_H_
