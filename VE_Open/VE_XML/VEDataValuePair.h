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
